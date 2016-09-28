/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

open Jenga_lib.Api;

open Merlin;

open NpmDep;

open Utils;

let jsOutput =
  (targets == [] ? ["index"] : targets) |>
  List.map f::(fun t => rel dir::(rel dir::buildDirRoot (tsl topLibName)) (t ^ ".js"));

/* Wrapper for the CLI `ocamldep`. Take the output, process it a bit, and pretend we've just called a regular
   ocamldep OCaml function. Note: the `ocamldep` utility doesn't give us enough info for fine, accurate module
   tracking in the presence of `open` */
let ocamlDep sourcePath::sourcePath => {
  let flag = isInterface sourcePath ? "-intf" : "-impl";
  /* seems like refmt intelligently detects source code type (re/ml) */
  let getDepAction () =>
    bashf
      "ocamldep -pp refmt -ppx node_modules/.bin/bsppx -ml-synonym .re -mli-synonym .rei -modules -one-line %s %s 2>&1; (exit ${PIPESTATUS[0]})"
      flag
      (tsp sourcePath);
  let action = Dep.action_stdout (Dep.path sourcePath |> mapD getDepAction);
  let processRawString string =>
    switch (String.strip string |> String.split on::':') {
    | [original, deps] => (
        rel dir::Path.the_root original,
        String.split deps on::' ' |> List.filter f::nonBlank |> List.map f::(fun m => Mod m)
      )
    | _ => failwith "expected exactly one ':' in ocamldep output line"
    };
  Dep.map action processRawString
};

/* Get only the dependencies on sources in the current library. */
let ocamlDepCurrentSources sourcePath::sourcePath paths::paths =>
  ocamlDep sourcePath::sourcePath |>
  mapD (
    fun (original, deps) => {
      let originalModule = pathToModule original;
      /* Dedupe, because we might have foo.re and foo.rei */
      let sourceModules = List.map paths f::pathToModule |> List.dedup;
      /* If the current file's Foo.re, and it depend on Foo, then it's certainly not depending on
         itself, which means that Foo either comes from a third-party module (which we can ignore
         here), or is a nested module from an `open`ed module, which ocamldep would have detected and
         returned in this list. */
      List.filter deps f::(fun m => m != originalModule) |>
      List.filter f::(fun m => List.exists sourceModules f::(fun m' => m == m'))
    }
  );

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
let moduleAliasFileScheme buildDir::buildDir sourcePaths::sourcePaths libName::libName => {
  let name ext => rel dir::buildDir (tsm (libToModule libName) ^ ext);
  let sourcePath = name ".ml";
  let targets = [".cmj", ".cmi", ".cmt"] |> List.map f::name;

  /** We omit interface to create the alias file  */
  let sourceNotInterfacePaths = List.filter sourcePaths f::(fun path => not (isInterface path));
  let fileContent =
    sourceNotInterfacePaths |>
    List.map
      f::(
        fun path =>
          Printf.sprintf
            "module %s = %s\n"
            (tsm (pathToModule path))
            (namespacedName libName::libName path::path)
      ) |>
    String.concat sep::"";
  let action =
    bashf
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
      (Path.to_string (name ".cmj"));
  /* TODO: do we even need the cmj file here? */
  let compileRule = Rule.create targets::targets (Dep.path sourcePath |> mapD (fun () => action));
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
  /* compiling here only needs cmis. If the interface signature doesn't change, ocaml doesn't need
     to recompile the dependent modules. Win. */
  let thirdPartyNpmLibs = getThirdPartyNpmLibs libDir::libDir;
  let thirdPartyOcamlfindLibNames = getThirdPartyOcamlfindLibs libDir::libDir;

  /** FIXME Doesn't work with core_kernel **/
  let ocamlfindPackagesStr =
    switch thirdPartyOcamlfindLibNames {
    | [] => ""
    | libs => "-bs-package-include " ^ (libs |> List.map f::tsl |> String.concat sep::",")
    };

  /** Compute Module Alias dependencies for dependencies only */
  let moduleAliasDep = Dep.all_unit (
    (isTopLevelLib ? [] : ["cmi", "cmj", "cmt"]) |>
    List.map f::(fun extension => relD dir::buildDir (tsm (libToModule libName) ^ "." ^ extension))
  );

  /** Compiling the current source file depends on all of the cmis of all its third-party libraries'
      source files being compiled. This is very coarse since in reality, we only depend on a few source
      files of these third-party libs. But ocamldep isn't granular enough to give us this information
      yet. */
  let thirdPartiesArtifactsDep = Dep.all_unit (
    List.map
      thirdPartyNpmLibs
      f::(
        fun libName => {
          /* if one of a third party library foo's source is Hi.re, then it resides in
             `node_modules/foo/src/Hi.re`, and its cmj artifacts in `_build/foo/Foo__Hi.cmj` */
          let thirdPartySrcPath = rel dir::(rel dir::nodeModulesRoot (tsl libName)) "src";
          let thirdPartyBuildPathD path::path ext::ext =>
            relD
              dir::(rel dir::buildDirRoot (tsl libName))
              (namespacedName libName::libName path::path ^ ext);

          /** FIXME Temporary workaround for bucklescript bug */
          let bsThirdPartyBuildPathD path::path ext::ext =>
            relD
              dir::(rel dir::buildDirRoot (tsl libName))
              (bsNamespacedName libName::libName path::path ^ ext);

          /** No need to glob `.rei/.mli`s here. We're only getting the file names to
              construct cmj paths. We depend on cmj artifacts rather cmi artifacts because
              cmi articfacts can be just generated with interface files and it is not sufficient
              to build the target */
          /* FIXME the js dependencies are Temporary and can just be cmj dependencies once
             bucklescript bug is removed */
          Dep.all (getSourceFiles dir::thirdPartySrcPath |> List.map f::Dep.return) |>
          bindD (
            fun thirdPartySources => Dep.all_unit (
              List.map
                thirdPartySources f::(fun path => thirdPartyBuildPathD path::path ext::".cmj") @
              List.map
                thirdPartySources f::(fun path => bsThirdPartyBuildPathD path::path ext::".js")
            )
          )
        }
      )
  );

  /** Compute build graph (targets, dependencies) for the current path */
  let compilePathScheme path =>
    ocamlDepCurrentSources sourcePath::path paths::sourcePaths |>
    mapD (
      fun firstPartyDeps => {
        let isInterface' = isInterface path;
        let hasInterface' = hasInterface sourcePaths::sourcePaths path;

        /** Helper functions to generate build dir paths **/
        let namespacedPath ext =>
          rel dir::buildDir (namespacedName libName::libName path::path ^ ext);
        let simplePath ext => rel dir::buildDir (fileNameNoExtNoDir path ^ ext);

        /** flag to include all the dependencies build dir's **/
        let includeDir =
          thirdPartyNpmLibs |> List.map f::(fun libName => "-I _build/" ^ tsl libName) |>
          String.concat sep::" ";

        /** Debug Info */
        /* print_endline ("Path: " ^ tsp path);
           print_endline "First Party Deps: ";
           print_endline ("Had Interface: " ^ string_of_bool hasInterface');
           print_endline ("Build Dir: " ^ tsp buildDir);
           print_endline ("Lib Dir: " ^ tsp libDir); */

        /** Rule for compiling .re/rei/ml/mli to .js **/
        /*
           More Flags:
             -bs-package-output  set npm-output-path: [opt_module]:path, for example: 'lib/cjs', 'amdjs:lib/amdjs' and 'goog:lib/gjs'

             -bs-package-name is set `self` to that it produces right require calls.
         */
        let action =
          bashf
            (
              if isInterface' {
                "bsc -g %s -pp refmt -bs-package-name self -bs-package-output commonjs:%s %s %s -o %s -c -intf %s 2>&1; (exit ${PIPESTATUS[0]})"
              } else if (
                hasInterface' && String.is_suffix (Path.basename path) suffix::".re"
              ) {
                "bsc -g %s -pp refmt -bin-annot -bs-package-name self -bs-package-output commonjs:%s %s %s -o %s -c -intf-suffix .rei -impl %s 2>&1; (exit ${PIPESTATUS[0]})"
              } else {
                "bsc -g %s -pp refmt -bin-annot -bs-package-name self -bs-package-output commonjs:%s %s %s -o %s -c -impl %s 2>&1; (exit ${PIPESTATUS[0]})"
              }
            )
            ocamlfindPackagesStr
            (tsp buildDir)
            (isTopLevelLib ? "" : "-open " ^ tsm (libToModule libName))
            (includeDir ^ " -I " ^ tsp buildDir)
            (isTopLevelLib ? tsp (simplePath "") : tsp (namespacedPath ""))
            (tsp path);

        /** Compute the artifacts extensions generate for each file type and then generate the
            correct build dir path for them **/
        /* FIXME Due to BuckleScript bug #757, we around it by using simplePath **/
        let targets =
          if isInterface' {
            [isTopLevelLib ? simplePath ".cmi" : namespacedPath ".cmi"]
          } else {
            let extns =
              if hasInterface' {
                [".cmj", ".cmt"]
              } else {
                [".cmi", ".cmj", ".cmt"]
              };
            (isTopLevelLib ? List.map f::simplePath extns : List.map f::namespacedPath extns) @ [
              simplePath ".js"
            ]
          };

        /** FIXME is this comment valid?
            compiling here only needs cmjs. If the interface signature doesn't change, ocaml doesn't need
            to recompile the dependent modules. Win. */
        let firstPartyArtifactDeps =
          sourcePaths |>
          List.filter
            f::(fun path => List.exists firstPartyDeps f::(fun m => m == pathToModule path)) |>
          List.map f::(fun path => relD dir::buildDir (fileNameNoExtNoDir path ^ ".cmj"));
        let firstPartyArtifactDeps =
          if hasInterface' {
            [
              /* We're a source file with an interface; include our own cmi as a dependency (our interface
                 file should compile before ourselves). */
              relD dir::buildDir (fileNameNoExtNoDir path ^ ".cmi"),
              ...firstPartyArtifactDeps
            ]
          } else {
            firstPartyArtifactDeps
          };

        /** The overall dependecies include the cmj artifacts of the both self and third party
            and interface artifact if an interface exits **/
        let deps = Dep.all_unit [
          Dep.path path,
          thirdPartiesArtifactsDep,
          moduleAliasDep,
          ...firstPartyArtifactDeps
        ];

        /** Workaround for BuckleScript bug https://github.com/bloomberg/bucklescript/issues/757
            Action to copy the file to match require call */
        let copyTarget = rel dir::buildDir (bsNamespacedName libName::libName path::path ^ ".js");
        let copyAction = bashf "cp %s %s" (tsp (simplePath ".js")) (tsp copyTarget);
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
  Dep.all (getSourceFiles dir::srcDir |> List.map f::Dep.return) |>
  mapD (
    fun unsortedPaths => {
      /* To prevent name collisions between src files and other modules,
         we namespace all modules except the top level module */
      let moduleAliasScheme =
        not isTopLevelLib ? [moduleAliasFileScheme buildDir unsortedPaths libName] : [];
      Scheme.all (
        moduleAliasScheme @ [
          compileSourcesScheme
            libDir::(Path.dirname srcDir)
            buildDir::buildDir
            libName::libName
            sourcePaths::unsortedPaths
            isTopLevelLib::isTopLevelLib
        ]
      )
    }
  ) |> Scheme.dep;

let scheme dir::dir =>
  if (dir == Path.the_root) {
    Scheme.all [Scheme.rules [Rule.default dir::dir (List.map f::Dep.path jsOutput)]]
  } else if (
    Path.is_descendant dir::buildDirRoot dir
  ) {
    let dirName = Path.basename dir;
    let libName = Lib (Path.basename dir);
    let isTopLevelLib = libName == topLibName;
    let srcDir = isTopLevelLib ? topSrcDir : rel dir::(rel dir::nodeModulesRoot dirName) "src";
    compileLibScheme srcDir::srcDir isTopLevelLib::isTopLevelLib libName::libName buildDir::dir
  } else {
    Scheme.no_rules
  };
