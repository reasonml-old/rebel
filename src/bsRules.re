open Core.Std;

open Jenga_lib.Api;

let bindD' f dep => Dep.bind dep f;

let mapD' f dep => Dep.map dep f;

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
  let action = Dep.action_stdout (Dep.path sourcePath |> mapD' getDepAction);
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
  bindD' (
    fun (original, deps) =>
      Dep.glob_listing (Glob.create dir::srcDir "*.{re,rei,ml,mli}") |>
      mapD' (
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

let compileSourcesScheme
    libDir::libDir
    buildDir::buildDir
    libName::libName
    sourcePaths::sourcePaths => {
  let compilePathScheme path =>
    ocamlDepCurrentSources sourcePath::path |>
    mapD' (
      fun firstPartyDeps => {
        /* compiling here only needs cmis. If the interface signature doesn't change, ocaml doesn't need
           to recompile the dependent modules. Win. */
        let isInterface' = Utils.isInterface path;
        let hasInterface =
          not isInterface' &&
          List.exists
            sourcePaths
            f::(
              fun path' =>
                Utils.isInterface path' &&
                Utils.fileNameNoExtNoDir path' == Utils.fileNameNoExtNoDir path
            );
        let firstPartyCmisDeps =
          sourcePaths |>
          List.filter
            f::(
              fun path => {
                let pathAsModule = Utils.pathToModule path;
                List.exists firstPartyDeps f::(fun m => m == pathAsModule)
              }
            ) |>
          List.map
            f::(fun path => Utils.relD dir::buildDir (Utils.fileNameNoExtNoDir path ^ ".cmi"));
        let firstPartyCmisDeps =
          if (not isInterface' && hasInterface) {
            [
              /* We're a source file with an interface; include our own cmi as a dependency (our interface
                 file should be compile before ourselves). */
              Utils.relD dir::buildDir (Utils.fileNameNoExtNoDir path ^ ".cmi"),
              ...firstPartyCmisDeps
            ]
          } else {
            firstPartyCmisDeps
          };
        let fileName ext::ext => Path.relative dir::buildDir (Utils.fileNameNoExtNoDir path ^ ext);
        let jsp = fileName ".js";
        let cmi = fileName ".cmi";
        let cmj = fileName ".cmj";
        let cmt = fileName ".cmt";
        let action =
          Utils.bashf
            /*
                More Flags:
               -bs-package-output  set npm-output-path: [opt_module]:path, for example: 'lib/cjs', 'amdjs:lib/amdjs' and 'goog:lib/gjs'

               -bs-package-name is set `self` to that it produces right require calls.
             */
            "bsc -g -pp refmt -bin-annot -bs-package-name self -bs-package-output commonjs:%s -I %s -o %s -c -impl %s"
            (Utils.tsp buildDir)
            (Utils.tsp buildDir)
            (Utils.tsp jsp)
            (Utils.tsp path);
        let targets = [cmi, cmj, cmt, jsp];
        let deps = Dep.all_unit firstPartyCmisDeps;
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
  mapD' (
    fun unsortedPaths =>
      /* List.iter unsortedPaths f::(fun x => print_endline (Utils.tsp x)); */
      compileSourcesScheme
        libDir::(Path.dirname srcDir)
        buildDir::buildDir
        libName::libName
        sourcePaths::unsortedPaths
  ) |> Scheme.dep;

let bsScheme dir::dir =>
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
