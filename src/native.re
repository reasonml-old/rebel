/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Utils;

let module List = Core.Std.List;
let module String = Core.Std.String;

let module Dep = Jenga_lib.Api.Dep;
let module Path = Jenga_lib.Api.Path;
let module Glob = Jenga_lib.Api.Glob;
let module Rule = Jenga_lib.Api.Rule;
let module Action = Jenga_lib.Api.Action;
let module Scheme = Jenga_lib.Api.Scheme;

let libraryFileName = "lib.cma";

let binaryOutput = rel dir::(rel dir::buildDirRoot (tsl topLibName)) "app.out";

let jsOutput = rel dir::(rel dir::buildDirRoot (tsl topLibName)) "app.js";

/* Wrapper for the CLI `ocamldep`. Take the output, process it a bit, and pretend we've just called a regular
   ocamldep OCaml function. Note: the `ocamldep` utility doesn't give us enough info for fine, accurate module
   tracking in the presence of `open` */
let ocamlDep sourcePath::sourcePath => {
  let flag = isInterface sourcePath ? "-intf" : "-impl";
  /* seems like refmt intelligently detects source code type (re/ml) */
  let getDepAction () =>
    bashf
      "ocamldep -pp refmt -ml-synonym .re -mli-synonym .rei -modules -one-line %s %s 2>&1; (exit ${PIPESTATUS[0]})"
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
let ocamlDepCurrentSources sourcePath::sourcePath => {
  let srcDir = Path.dirname sourcePath;
  ocamlDep sourcePath::sourcePath |>
  bindD (
    fun (original, deps) =>
      Dep.glob_listing (Glob.create dir::srcDir "*.{re,rei,ml,mli}") |>
      mapD (
        fun sourcePaths => {
          let originalModule = pathToModule original;
          /* Dedupe, because we might have foo.re and foo.rei */
          let sourceModules = List.map sourcePaths f::pathToModule |> List.dedup;
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

/* Used to compile a library file. The compile command requires files to be passed in order. If A requires B
   but B is passed after A in the command, the compilation will fail with e.g. "module B not found" when
   compiling A */
let sortPathsTopologically paths::paths => {
  let pathsAsModulesOriginalCapitalization =
    List.map paths f::(fun path => (pathToModule path, path));
  let pathsAsModules = List.map pathsAsModulesOriginalCapitalization f::fst;
  let moduleDepsForPathsD =
    paths |> List.map f::(fun path => ocamlDepCurrentSources sourcePath::path) |> Dep.all;
  moduleDepsForPathsD |>
  mapD (
    fun moduleDepsForPaths =>
      List.zip_exn pathsAsModules moduleDepsForPaths |> topologicalSort |>
      List.map f::(fun m => List.Assoc.find_exn pathsAsModulesOriginalCapitalization m)
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
let moduleAliasFileScheme buildDir::buildDir sourcePaths::sourcePaths libName::libName => {
  let moduleAliasFile extension => rel dir::buildDir (tsm (libToModule libName) ^ extension);
  let sourcePath = moduleAliasFile ".ml";
  let targets = [".cmo", ".cmi", ".cmt"] |> List.map f::moduleAliasFile;

  /** We omit interface to create the alias file  */
  let sourceNotInterfacePaths = List.filter sourcePaths f::(fun path => not (isInterface path));
  let fileContent =
    List.map
      sourceNotInterfacePaths
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
      "ocamlc -custom -bin-annot -g -no-alias-deps -w -49 -w -30 -w -40 -c -impl %s -o %s 2>&1; (exit ${PIPESTATUS[0]})"
      (tsp sourcePath)
      (tsp (moduleAliasFile ".cmo"));
  /* TODO: do we even need the cmo file here? */
  let compileRule =
    Rule.create targets::targets (Dep.map (Dep.path sourcePath) (fun () => action));
  let contentRule =
    Rule.create targets::[sourcePath] (Dep.return (Action.save fileContent target::sourcePath));
  Scheme.rules [contentRule, compileRule]
};

/* We compile each file in the current library (say, foo). If a file's Bar.re, it'll be compiled to
   foo__Bar.{cmi, cmo, cmt}. As to why we're namespacing compiled outputs like this, see
   `moduleAliasFileScheme`. */
let compileSourcesScheme
    libDir::libDir
    buildDir::buildDir
    libName::libName
    sourcePaths::sourcePaths
    isTopLevelLib::isTopLevelLib => {
  /* This is the module alias file generated through `moduleAliasFileScheme`, that we said we're gonna `-open`
     during `ocamlc` */
  let thirdPartyNpmLibs = NpmDep.getThirdPartyNpmLibs libDir::libDir;
  let thirdPartyOcamlfindLibNames = NpmDep.getThirdPartyOcamlfindLibs libDir::libDir;

  /** Compute Module Alias dependencies for dependencies only */
  let moduleAliasDep = Dep.all_unit (
    (isTopLevelLib ? [] : [".cmo", ".cmi", ".cmt", ".ml"]) |>
    List.map f::(fun ext => relD dir::buildDir (tsm (libToModule libName) ^ ext))
  );

  /** TODO Make this work with core */
  let ocamlfindPackagesStr =
    switch thirdPartyOcamlfindLibNames {
    | [] => ""
    | libs => "-package " ^ (libs |> List.map f::tsl |> String.concat sep::",")
    };

  /** Compiling the current source file depends on all of the cmis of all its third-party libraries'
      source files being compiled. This is very coarse since in reality, we only depend on a few source
      files of these third-party libs. But ocamldep isn't granular enough to give us this information
      yet. */
  let thirdPartiesCmosDep = Dep.all_unit (
    List.map
      thirdPartyNpmLibs
      f::(
        fun libName => {
          /* if one of a third party library foo's source is Hi.re, then it resides in
             `node_modules/foo/src/Hi.re`, and its cmo artifacts in `_build/foo/Foo__Hi.cmo` */
          let thirdPartySrcPath = rel dir::(rel dir::nodeModulesRoot (tsl libName)) "src";
          let thirdPartyBuildPathD path::path ext::ext =>
            relD
              dir::(rel dir::buildDirRoot (tsl libName))
              (namespacedName libName::libName path::path ^ ext);

          /** No need to glob `.rei/.mli`s here. We're only getting the file names to
              construct cmi paths.We depend onf cmo artifacts rather cmi artifacts because
              cmi articfacts can be just generated with interface files and it is not sufficient
              to build the target */
          Dep.glob_listing (Glob.create dir::thirdPartySrcPath "*.{re,ml}") |>
          bindD (
            fun thirdPartySources => Dep.all_unit (
              List.map
                thirdPartySources f::(fun path => thirdPartyBuildPathD path::path ext::".cmo")
            )
          )
        }
      )
  );

  /** Compute build graph (targets, dependencies) for the current path */
  let compileEachSourcePath path =>
    ocamlDepCurrentSources sourcePath::path |>
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

        /** Hard Coded Rules for special packages */
        let extraFlags =
          if (List.mem thirdPartyOcamlfindLibNames (Lib "core")) {
            "-thread -package threads"
          } else {
            ""
          };

        /** Debug Info */
        /* print_endline ("Path: " ^ tsp path);
           print_endline "First Party Deps: ";
           print_endline ("Had Interface: " ^ string_of_bool hasInterface');
           print_endline ("Build Dir: " ^ tsp buildDir);
           print_endline ("Lib Dir: " ^ tsp libDir); */

        /** Rule for compiling .re/rei/ml/mli to .cmo **/
        /*
         Most of the flags here have been explained previously in `moduleAliasFileScheme`.
         -intf-suffix: tells ocamlc what the interface file's extension is.

         -c: compile only, don't link yet.
         Example command: ocamlc -pp refmt -bin-annot -g -w -30 -w -40 -open Foo -I \
         path/to/js_of_ocaml path/to/js_of_ocaml/js_of_ocaml.cma -I ./ -I ../fooDependsOnMe -I \
         ../fooDependsOnMeToo -o foo__CurrentSourcePath -intf-suffix .rei -c -impl \
         path/to/CurrentSourcePath.re */
        let action =
          bashf
            (
              if isInterface' {
                "ocamlfind ocamlc -custom -pp refmt -g -w -30 -w -40 %s -I %s %s %s %s -o %s -c -intf %s 2>&1; (exit ${PIPESTATUS[0]})"
              } else if (
                hasInterface' && String.is_suffix (Path.basename path) suffix::".re"
              ) {
                "ocamlfind ocamlc -custom -pp refmt -bin-annot -g -w -30 -w -40 %s -I %s %s %s %s -o %s -c -intf-suffix .rei -impl %s 2>&1; (exit ${PIPESTATUS[0]})"
              } else {
                "ocamlfind ocamlc -custom -pp refmt -bin-annot -g -w -30 -w -40 %s -I %s %s %s %s -o %s -c -impl %s 2>&1; (exit ${PIPESTATUS[0]})"
              }
            )
            (isTopLevelLib ? "" : "-open " ^ tsm (libToModule libName))
            (tsp buildDir)
            extraFlags
            ocamlfindPackagesStr
            includeDir
            (isTopLevelLib ? tsp (simplePath "") : tsp (namespacedPath ""))
            (tsp path);

        /** compiling here only needs cmis. If the interface signature doesn't change, ocaml doesn't need
            to recompile the dependent modules. Win. */
        let firstPartyArtifactDeps =
          sourcePaths |>
          List.filter
            f::(fun path => List.exists firstPartyDeps f::(fun m => m == pathToModule path)) |>
          List.map f::(fun path => relD dir::buildDir (fileNameNoExtNoDir path ^ ".cmo"));
        let firstPartyArtifactDeps =
          if (not isInterface' && hasInterface') {
            [
              /* We're a source file with an interface; include our own cmi as a dependency (our interface
                 file should be compile before ourselves). */
              relD dir::buildDir (fileNameNoExtNoDir path ^ ".cmi"),
              ...firstPartyArtifactDeps
            ]
          } else {
            firstPartyArtifactDeps
          };

        /** The overall dependecies include the js artifacts of the both self and third party
            and interface artifact if an interface exits **/
        let deps = Dep.all_unit [
          Dep.path path,
          moduleAliasDep,
          thirdPartiesCmosDep,
          ...firstPartyArtifactDeps
        ];

        /** Compute the artifacts extensions generate for each file type and then generate the
            correct build dir path for them **/
        let targets = {
          let extns =
            if isInterface' {
              [".cmi"]
            } else if hasInterface' {
              [".cmo", ".cmt"]
            } else {
              [".cmi", ".cmo", ".cmt"]
            };
          isTopLevelLib ? List.map f::simplePath extns : List.map f::namespacedPath extns
        };
        Rule.create targets::targets (Dep.map deps (fun () => action))
      }
    );
  Scheme.rules_dep (Dep.all (List.map sourcePaths f::compileEachSourcePath))
};

/* This function assumes we're not using it at the top level.
   Cma is a library file for the current library which bundles up all the lib's first-party compiled sources.
   This way, it's much easier, at the end, at the top level, to include each library's cma file to compile the
   final executable, than to tediously pass every single source file from every lib in order.

   There's a caveat though. We said we're only bundling up the current lib's first-party code; We _could_ have
   bundled up its third-party deps' cma too, but then we might get into duplicate artifact problem caused by
   e.g. library A and B both requiring and bundling C. So we can only bundle first-party code, and then, at
   the top, take all the transitive dependencies (libraries) cmas, figure out their relative order, and pass
   them in that order to the ocamlc command (this logic is in `finalOutputsScheme` below). Still tedious, but
   at least we're not passing individual source files in order. */
let compileCmaScheme sortedSourcePaths::sortedSourcePaths libName::libName buildDir::buildDir => {
  let moduleName = tsm (libToModule libName);
  let cmaPath = rel dir::buildDir libraryFileName;
  let moduleAliasCmoPath = rel dir::buildDir (moduleName ^ ".cmo");
  let cmos =
    List.map
      /* To compile one cma file, we need to pass the compiled first-party sources in order to ocamlc */
      sortedSourcePaths
      f::(fun path => rel dir::buildDir (namespacedName libName::libName path::path ^ ".cmo"));
  let cmosString = List.map cmos f::tsp |> String.concat sep::" ";
  /* Final bundling. Time to get all the transitive dependencies... */
  Scheme.rules [
    Rule.simple
      targets::[cmaPath]
      deps::(List.map [moduleAliasCmoPath, ...cmos] f::Dep.path)
      action::(
        bashf
          /* Flags:
             -open: compile the file as if [file being opened] was opened at the top of the file. In
             our case, we open our module alias file generated with `moduleAliasFileScheme`. See that
             function for more comment.

             -a: flag for building a library.

             -o: output file name.
             */
          /* Example command: ocamlc -g -open Foo -a -o lib.cma foo.cmo aDependsOnMe.cmo a.cmo b.cmo */
          "ocamlc -custom -g -open %s -a -o %s %s %s 2>&1; (exit ${PIPESTATUS[0]})"
          moduleName
          (tsp cmaPath)
          (tsp moduleAliasCmoPath)
          cmosString
      )
  ]
};

/* This function assumes we're using it only at the top level.
   We'll output an executable, plus a js_of_ocaml JavaScript file. Throughout the compilation of the source
   files, we've already mingled in the correctly jsoo search paths in ocamlc to make this final compilation
   work. */
let finalOutputsScheme sortedSourcePaths::sortedSourcePaths => {
  let buildDir = rel dir::buildDirRoot (tsl topLibName);
  /* let moduleAliasCmoPath = rel dir::buildDir (tsm (libToModule topLibName) ^ ".cmo"); */
  let cmos =
    List.map
      /* To compile one cma file, we need to pass the compiled first-party sources in order to ocamlc */
      sortedSourcePaths f::(fun path => rel dir::buildDir (fileNameNoExtNoDir path ^ ".cmo"));
  let cmosString = List.map cmos f::tsp |> String.concat sep::" ";
  let transitiveCmaPaths =
    List.map
      NpmDep.sortedTransitiveThirdPartyNpmLibsIncludingSelf's
      f::(fun libName => rel dir::(rel dir::buildDirRoot (tsl libName)) libraryFileName);
  let ocamlfindPackagesStr =
    if (NpmDep.transitiveThirdPartyOcamlfindLibsIncludingSelf's == []) {
      ""
    } else {
      "-linkpkg -package " ^ (
        List.map NpmDep.transitiveThirdPartyOcamlfindLibsIncludingSelf's f::tsl |> String.concat sep::","
      )
    };

  /** Hard Coded Rules for special packages */
  let extraFlags =
    if (List.mem NpmDep.transitiveThirdPartyOcamlfindLibsIncludingSelf's (Lib "core")) {
      "-thread -package threads"
    } else {
      ""
    };
  let action =
    bashf
      /* For ease of coding, we'll blindly include js_of_ocaml in the -I search path here, in case
         the module invokes some jsoo's Js module-related stuff. */
      /* Example command: ocamlc -g -I path/to/js_of_ocaml path/to/js_of_ocaml/js_of_ocaml.cma \
         -open Top -o app.out  ../barDependsOnMe/lib.cma ../bar/lib.cma ../baz/lib.cma \
         top.cmo aDependsOnMe.cmo a.cmo moreFirstPartyCmo.cmo */
      /* Flags:
         -I: search path(s), when ocamlc looks for modules referenced inside the file.

         -open: compile the file as if [file being opened] was opened at the top of the file. In
         our case, we open our module alias file generated with `moduleAliasFileScheme`. See that
         function for more comment.

         -o: output file name.
         */
      "ocamlfind ocamlc -custom %s %s -g -o %s %s %s 2>&1; (exit ${PIPESTATUS[0]})"
      extraFlags
      ocamlfindPackagesStr
      (tsp binaryOutput)
      (transitiveCmaPaths |> List.map f::tsp |> String.concat sep::" ")
      /* (tsp moduleAliasCmoPath) */
      cmosString;
  let nativeRule =
    /* We check here for jsoo because jsoo needs binaryOutput */
    backend == "native" || backend == "jsoo" ?
      [
        Rule.simple
          targets::[binaryOutput]
          deps::(
            /* TODO: I don't think cmis and cmts are being read here, so we don't need to include them. */
            cmos @ transitiveCmaPaths |> List.map f::Dep.path
          )
          action::action
      ] :
      [];
  let javascriptRule =
    backend == "jsoo" ?
      [
        Rule.simple
          targets::[jsOutput]
          deps::[Dep.path binaryOutput]
          action::(
            bashf
              /* I don't know what the --linkall flag does, and does the --pretty flag work? Because the
                 output is still butt ugly. Just kidding I love you guys. */
              "js_of_ocaml --source-map --no-inline --debug-info --pretty --linkall -o %s %s"
              (tsp jsOutput)
              (tsp binaryOutput)
          )
      ] :
      [];
  Scheme.rules (List.append nativeRule javascriptRule)
};

/* The function that ties together all the previous steps and compiles a given library, whether it be our top
   level library or a third-party one. */
let compileLibScheme
    isTopLevelLib::isTopLevelLib=true
    srcDir::srcDir
    libName::libName
    buildDir::buildDir =>
  Dep.glob_listing (Glob.create dir::srcDir "*.{re,rei,ml,mli}") |>
  bindD (
    fun unsortedPaths =>
      sortPathsTopologically paths::unsortedPaths |>
      mapD (
        fun sortedPaths => Scheme.all [
          moduleAliasFileScheme buildDir::buildDir libName::libName sourcePaths::unsortedPaths,
          compileSourcesScheme
            libDir::(Path.dirname srcDir)
            buildDir::buildDir
            libName::libName
            sourcePaths::unsortedPaths
            isTopLevelLib::isTopLevelLib,
          isTopLevelLib ?
            /* if we're at the final, top level compilation, there's no need to build a cma output (and
               then generate an executable from it). We can cut straight to generating the executable. See
               `finalOutputsScheme`. */
            finalOutputsScheme sortedSourcePaths::sortedPaths :
            compileCmaScheme buildDir::buildDir libName::libName sortedSourcePaths::sortedPaths
        ]
      )
  ) |> Scheme.dep;

let scheme dir::dir => {
  ignore dir;
  /* We generate many .merlin files, one per third-party library (and on at the top). Additionally, this is
     the only case where we generate some artifacts outside of _build/. Most of this is so that Merlin's
     jump-to-location could work correctly when we jump into a third-party source file. As to why exactly we
     generate .merlin with the content that it is, call 1-800-chenglou-plz-help. */
  if (dir == Path.the_root) {
    let defaultRule =
      switch backend {
      | "jsoo" => [Dep.path jsOutput]
      | "native" => [Dep.path binaryOutput]
      | _ => []
      };
    Scheme.all [Scheme.rules [Rule.default dir::dir defaultRule]]
  } else if (
    Path.is_descendant dir::buildDirRoot dir
  ) {
    let dirName = Path.basename dir;
    let libName = Lib (Path.basename dir);
    let isTopLevelLib = libName == topLibName;
    let srcDir = isTopLevelLib ? topSrcDir : rel dir::(rel dir::nodeModulesRoot dirName) "src";
    compileLibScheme
      srcDir::srcDir
      isTopLevelLib::isTopLevelLib
      libName::libName
      buildDir::(rel dir::buildDirRoot dirName)
  } else if (
    Path.dirname dir == nodeModulesRoot
  ) {
    let libName = Lib (Path.basename dir);
    Merlin.dotMerlinScheme isTopLevelLib::false dir::dir libName::libName bscBackend::false
  } else {
    Scheme.no_rules
  }
};
