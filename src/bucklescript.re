open Core.Std;

open Jenga_lib.Api;

let bindD f dep => Dep.bind dep f;

let mapD f dep => Dep.map dep f;

let jsOutput =
  Path.relative
    dir::(Path.relative dir::Utils.buildDirRoot (Utils.tsl Utils.topLibName)) "Index.js";

/* Wrapper for the CLI `ocamldep`. Take the output, process it a bit, and pretend we've just called a regular
   ocamldep OCaml function. Note: the `ocamldep` utility doesn't give us enough info for fine, accurate module
   tracking in the presence of `open` */
let ocamlDep sourcePath::sourcePath => {
  let flag = Utils.isInterface sourcePath ? "-intf" : "-impl";
  let getDepAction () =>
    Utils.bashf
      /* seems like refmt intelligently detects source code type (re/ml) */
      "ocamldep -pp refmt -ppx node_modules/.bin/bsppx -ml-synonym .re -mli-synonym .rei -modules -one-line %s %s 2>&1; (exit ${PIPESTATUS[0]})"
      flag
      (Path.to_string sourcePath);
  let action = Dep.action_stdout (Dep.path sourcePath |> mapD getDepAction);
  let processRawString string =>
    switch (String.strip string |> String.split on::':') {
    | [original, deps] => (
        Path.relative dir::Path.the_root original,
        String.split deps on::' ' |> List.filter f::Utils.nonBlank |>
        List.map f::(fun m => Utils.Mod m)
      )
    | _ => failwith "expected exactly one ':' in ocamldep output line"
    };
  Dep.map action processRawString
};

/* Get only the dependencies on sources in the current library. */
let ocamlDepCurrentSources sourcePath::sourcePath => {
  let srcDir = Path.dirname sourcePath;
  ocamlDep sourcePath::sourcePath |>
  bindD (
    fun (original, deps) =>
      Dep.glob_listing (Glob.create dir::srcDir "*.{re,rei,ml,mli}") |>
      mapD (
        fun sourcePaths => {
          let originalModule = Utils.pathToModule original;
          /* Dedupe, because we might have foo.re and foo.rei */
          let sourceModules = List.map sourcePaths f::Utils.pathToModule |> List.dedup;
          /* If the current file's Foo.re, and it depend on Foo, then it's certainly not depending on
             itself, which means that Foo either comes from a third-party module (which we can ignore
             here), or is a nested module from an `open`ed module, which ocamldep would have detected and
             returned in this list. */
          List.filter deps f::(fun m => m != originalModule) |>
          List.filter f::(fun m => List.exists sourceModules f::(fun m' => m == m'))
        }
      )
  )
};

/*
   FIXME Unlike for native compilation, we don't need to perform name spacing magic here. Correct?
 */
let compileSourcesScheme
    libDir::libDir
    buildDir::buildDir
    libName::libName
    sourcePaths::sourcePaths => {
  open Utils;
  let compilePathScheme path =>
    ocamlDepCurrentSources sourcePath::path |>
    mapD (
      fun firstPartyDeps => {
        /* compiling here only needs cmis. If the interface signature doesn't change, ocaml doesn't need
           to recompile the dependent modules. Win. */
        let thirdPartyNpmLibs = NpmDep.getThirdPartyNpmLibs libDir::libDir;
        /* TODO Add Ocaml Find Dep support */
        let thirdPartyOcamlfindLibNames = NpmDep.getThirdPartyOcamlfindLibs libDir::libDir;
        let isInterface' = isInterface path;
        let hasInterface =
          not isInterface' &&
          List.exists
            sourcePaths
            f::(
              fun path' => isInterface path' && fileNameNoExtNoDir path' == fileNameNoExtNoDir path
            );
        let firstPartyCmisDeps =
          sourcePaths |>
          List.filter
            f::(
              fun path => {
                let pathAsModule = pathToModule path;
                List.exists firstPartyDeps f::(fun m => m == pathAsModule)
              }
            ) |>
          List.map f::(fun path => relD dir::buildDir (fileNameNoExtNoDir path ^ ".cmi"));
        let firstPartyCmisDeps =
          if (not isInterface' && hasInterface) {
            [
              /* We're a source file with an interface; include our own cmi as a dependency (our interface
                 file should be compile before ourselves). */
              relD dir::buildDir (fileNameNoExtNoDir path ^ ".cmi"),
              ...firstPartyCmisDeps
            ]
          } else {
            firstPartyCmisDeps
          };
        /* Compiling the current source file depends on all of the cmis of all its third-party libraries'
           source files being compiled. This is very coarse since in reality, we only depend on a few source
           files of these third-party libs. But ocamldep isn't granular enough to give us this information
           yet. */
        let thirdPartiesCmisDep = Dep.all_unit (
          List.map
            thirdPartyNpmLibs
            f::(
              fun libName => {
                /* if one of a third party library foo's source is Hi.re, then it resides in
                   `node_modules/foo/src/Hi.re`, and its cmi artifacts in `_build/foo/Hi.cmi` */
                let thirdPartySrcPath =
                  Path.relative dir::(Path.relative dir::nodeModulesRoot (tsl libName)) "src";
                /* No need to glob `.rei/.mli`s here. We're only getting the file names to
                   construct cmi paths. */
                Dep.glob_listing (Glob.create dir::thirdPartySrcPath "*.{re,ml}") |>
                bindD (
                  fun thirdPartySources => Dep.all_unit (
                    List.map
                      thirdPartySources
                      f::(
                        fun sourcePath =>
                          relD
                            dir::(Path.relative dir::buildDirRoot (tsl libName))
                            (fileNameNoExtNoDir sourcePath ^ ".cmi")
                      )
                  )
                )
              }
            )
        );
        let name ext::ext => Path.relative dir::buildDir (fileNameNoExtNoDir path ^ ext);
        let jsp = name ".js";
        let cmi = name ".cmi";
        let cmj = name ".cmj";
        let cmt = name ".cmt";
        let includeDir =
          thirdPartyNpmLibs |> List.map f::(fun libName => "-I _build/" ^ tsl libName) |>
          String.concat sep::" ";
        let action =
          bashf
            /*
                More Flags:
               -bs-package-output  set npm-output-path: [opt_module]:path, for example: 'lib/cjs', 'amdjs:lib/amdjs' and 'goog:lib/gjs'

               -bs-package-name is set `self` to that it produces right require calls.
             */
            "bsc -g -pp refmt -bin-annot -bs-package-name self -bs-package-output commonjs:%s %s -o %s -c -impl %s"
            (tsp buildDir)
            (includeDir ^ " -I " ^ tsp buildDir)
            (tsp jsp)
            (tsp path);
        let targets = [cmi, cmj, cmt, jsp];
        let deps = Dep.all_unit [Dep.path path, thirdPartiesCmisDep, ...firstPartyCmisDeps];
        Rule.create targets::targets (Dep.map deps (fun () => action))
      }
    );
  Scheme.rules_dep (Dep.all (List.map sourcePaths f::compilePathScheme))
};

let compileLibScheme
    libName::libName
    isTopLevelLib::isTopLevelLib
    srcDir::srcDir
    buildDir::buildDir =>
  Dep.glob_listing (Glob.create dir::srcDir "*.{re,rei,ml,mli}") |>
  mapD (
    fun unsortedPaths =>
      /* List.iter unsortedPaths f::(fun x => print_endline (Utils.tsp x)); */
      compileSourcesScheme
        libDir::(Path.dirname srcDir)
        buildDir::buildDir
        libName::libName
        sourcePaths::unsortedPaths
  ) |> Scheme.dep;

let scheme dir::dir =>
  if (dir == Path.the_root) {
    Scheme.all [Scheme.rules [Rule.default dir::dir [Dep.path jsOutput]]]
  } else if (
    Path.is_descendant dir::Utils.buildDirRoot dir
  ) {
    let dirName = Path.basename dir;
    let libName = Utils.Lib (Path.basename dir);
    let isTopLevelLib = libName == Utils.topLibName;
    let srcDir =
      isTopLevelLib ?
        Utils.topSrcDir :
        Path.relative dir::(Path.relative dir::Utils.nodeModulesRoot dirName) "src";
    compileLibScheme
      srcDir::srcDir
      isTopLevelLib::isTopLevelLib
      libName::libName
      buildDir::(Path.relative dir::Utils.buildDirRoot dirName)
  } else {
    Scheme.no_rules
  };
