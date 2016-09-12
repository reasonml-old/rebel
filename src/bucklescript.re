/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

open Jenga_lib.Api;

let bindD f dep => Dep.bind dep f;

let mapD f dep => Dep.map dep f;

let jsOutput =
  Path.relative
    dir::(Path.relative dir::Utils.buildDirRoot (Utils.tsl Utils.topLibName)) "index.js";

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

/* the module alias file takes the current library foo's first-party sources, e.g. A.re, B.re, and turn them
   into a foo.ml file whose content is:
   module A = Foo__A;
   module B = Foo__B;

   We'll then compile this file into foo.cmi/cmo/cmt, and have it opened by default when compiling A.re and
   B.re (into foo_A and foo_B respectively) later. The effect is that, inside A.re, we can refer to B instead
   of Foo__B thanks to the pre-opened foo.ml. But when these files are used by other libraries (which aren't
   compiled with foo.re pre-opened of course), they won't see module A or B, only Foo__A and Foo__B, aka in
   practice, they simply won't see them. This effectively means we've implemented namespacing!

   Note that we're generating a ml file rather than re, because this rebel theoretically works on pure
   ocaml projects too, with no dep on reason. */
let moduleAliasFileScheme
    buildDir::buildDir
    sourceNotInterfacePaths::sourceNotInterfacePaths
    libName::libName => {
  let name extension =>
    Path.relative dir::buildDir (Utils.tsm (Utils.libToModule libName) ^ "." ^ extension);
  let sourcePath = name "ml";
  let cmj = name "cmj";
  let cmi = name "cmi";
  let cmt = name "cmt";
  let fileContent =
    List.map
      sourceNotInterfacePaths
      f::(
        fun path =>
          Printf.sprintf
            "module %s = %s\n"
            (Utils.tsm (Utils.pathToModule path))
            (Utils.namespacedName libName::libName path::path)
      ) |>
    String.concat sep::"";
  let action =
    Utils.bashf
      /* We suppress a few warnings here through -w.
         - 49: Absent cmi file when looking up module alias. Aka Foo__A and Foo__B's compiled cmis
         can't be found at the moment this module alias file is compiled. This is normal, since the
         module alias file is the first thing that's compiled (so that we can open it during
         compilation of A.re and B.re into Foo__A and Foo__B). Think of this as forward declaration.

         - 30: Two labels or constructors of the same name are defined in two mutually recursive
         types. I forgot...

         - 40: Constructor or label name used out of scope. I forgot too. Great comment huh?

         More flags:
         -pp refmt option makes ocamlc take our reason syntax source code and pass it through our
         refmt parser first, before operating on the AST.

         -bin-annot: generates cmt files that contains info such as types and bindings, for use with
         Merlin.

         -g: add debugging info. You don't really ever compile without this flag.

         -impl: source file. This flag's needed if the source extension isn't ml. I think.

         -o: output name
         */
      "bsc -bin-annot -g -no-alias-deps -w -49 -w -30 -w -40 -c -impl %s -o %s 2>&1; (exit ${PIPESTATUS[0]})"
      (Path.to_string sourcePath)
      (Path.to_string cmj);
  /* TODO: do we even need the cmo file here? */
  let compileRule =
    Rule.create targets::[cmj, cmi, cmt] (Dep.path sourcePath |> mapD (fun () => action));
  let contentRule =
    Rule.create targets::[sourcePath] (Dep.return (Action.save fileContent target::sourcePath));
  Scheme.rules [contentRule, compileRule]
};

/* We perform name spacing magic for only dependencies.*/
let compileSourcesScheme
    libDir::libDir
    buildDir::buildDir
    libName::libName
    sourcePaths::sourcePaths
    isTopLevelLib::isTopLevelLib => {
  open Utils;

  /** compiling here only needs cmis. If the interface signature doesn't change, ocaml doesn't need
      to recompile the dependent modules. Win. */
  let thirdPartyNpmLibs = NpmDep.getThirdPartyNpmLibs libDir::libDir;

  /** TODO Add Ocaml Find Dep support */
  let thirdPartyOcamlfindLibNames = NpmDep.getThirdPartyOcamlfindLibs libDir::libDir;

  /** Compute Module Alias dependencies for dependencies only */
  let moduleAliasDep = Dep.all_unit (
    (isTopLevelLib ? [] : ["cmi", "cmj", "cmt", "ml"]) |>
    List.map f::(fun extension => relD dir::buildDir (tsm (libToModule libName) ^ "." ^ extension))
  );

  /** Compute build graph (targets, dependencies) for the current path */
  let compilePathScheme path =>
    ocamlDepCurrentSources sourcePath::path |>
    mapD (
      fun firstPartyDeps => {
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

        /** Compiling the current source file depends on all of the cmis of all its third-party libraries'
            source files being compiled. This is very coarse since in reality, we only depend on a few source
            files of these third-party libs. But ocamldep isn't granular enough to give us this information
            yet. */
        let thirdPartiesJsAndCmisDep = Dep.all_unit (
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
                            (namespacedName libName::libName path::sourcePath ^ ".cmi")
                      ) @
                    List.map
                      thirdPartySources
                      f::(
                        fun sourcePath =>
                          relD
                            dir::(Path.relative dir::buildDirRoot (tsl libName))
                            (bsNamespacedName libName::libName path::sourcePath ^ ".js")
                      )
                  )
                )
              }
            )
        );
        let namespacedPath ext::ext =>
          Path.relative dir::buildDir (namespacedName libName::libName path::path ^ ext);
        let simplePath ext::ext => Path.relative dir::buildDir (fileNameNoExtNoDir path ^ ext);
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
            "bsc -g -pp refmt -bin-annot -bs-package-name self -bs-package-output commonjs:%s %s %s -o %s -c -impl %s"
            (tsp buildDir)
            (isTopLevelLib ? "" : "-open " ^ tsm (libToModule libName))
            (includeDir ^ " -I " ^ tsp buildDir)
            (isTopLevelLib ? tsp (simplePath "") : tsp (namespacedPath ""))
            (tsp path);
        print_endline (namespacedName libName::libName path::path);
        print_endline (tsp (simplePath ""));
        print_endline (tsp (namespacedPath ".js"));
        let targets =
          isTopLevelLib ?
            [simplePath ".cmi", simplePath ".cmj", simplePath ".cmt", simplePath ".js"] :
            [namespacedPath ".cmi", namespacedPath ".cmj", namespacedPath ".cmt", simplePath ".js"];
        let deps = Dep.all_unit [
          Dep.path path,
          thirdPartiesJsAndCmisDep,
          moduleAliasDep,
          ...firstPartyCmisDeps
        ];

        /** Workaround for BuckleScript bug https://github.com/bloomberg/bucklescript/issues/757  */
        let copyTarget = Path.relative dir::buildDir (bsNamespacedName libName::libName path::path ^ ".js");
        let copyAction =
          bashf
            "cp %s %s"
            (tsp (simplePath ".js"))
            (
              tsp copyTarget
            );
        let copyRule =
          Rule.create
            targets::[copyTarget] (Dep.path (simplePath ".js") |> mapD (fun () => copyAction));

        /** Compile JS from BuckleScript and copy the file to match require call */
        Scheme.rules [
           Rule.create targets::targets (Dep.map deps (fun () => action)),
           ...isTopLevelLib ? [] : [copyRule]
        ]
      }
    );
  Scheme.all (List.map sourcePaths f::(fun path => compilePathScheme path |> Scheme.dep))
};

let compileLibScheme
    libName::libName
    isTopLevelLib::isTopLevelLib
    srcDir::srcDir
    buildDir::buildDir =>
  Dep.glob_listing (Glob.create dir::srcDir "*.{re,rei,ml,mli}") |>
  mapD (
    fun unsortedPaths => {
      let sourceNotInterfacePaths =
        List.filter unsortedPaths f::(fun path => not (Utils.isInterface path));
      if isTopLevelLib {
        Scheme.all [
          compileSourcesScheme
            libDir::(Path.dirname srcDir)
            buildDir::buildDir
            libName::libName
            sourcePaths::unsortedPaths
            isTopLevelLib::isTopLevelLib
        ]
      } else {
        Scheme.all [
          moduleAliasFileScheme
            buildDir::buildDir libName::libName sourceNotInterfacePaths::sourceNotInterfacePaths,
          compileSourcesScheme
            libDir::(Path.dirname srcDir)
            buildDir::buildDir
            libName::libName
            sourcePaths::unsortedPaths
            isTopLevelLib::isTopLevelLib
        ]
      }
    }
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
