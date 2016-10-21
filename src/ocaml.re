/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

let module Dep = Jenga_lib.Api.Dep;

let module Path = Jenga_lib.Api.Path;

let module Glob = Jenga_lib.Api.Glob;

let module Rule = Jenga_lib.Api.Rule;

let module Action = Jenga_lib.Api.Action;

let module Scheme = Jenga_lib.Api.Scheme;

open Utils;

/* When we build src folder of any package, we convert the package name in camelCase and
   use it for modules alias. For the top level src, we simply module alias it with `Src`.

   Note: Libname is just last path component of buildDir here */
let moduleAliasFile buildDir::buildDir libName::libName ext::ext =>
  rel dir::buildDir (tsm (libToModule libName) ^ ext);

/* We treat each folder as a module/library.

   The module alias file takes the current library foo's first-party sources, e.g. A.re, C/B.re,
   and turn them into a Foo.ml/Foo_Foo.ml file whose content is:

   module A = Foo__A;
   module B = Foo__B;

   We'll then compile this file into foo.cmi/cmo/cmt, and have it opened by default when compiling A.re and
   B (into Foo__A and Foo__B respectively) later. The effect is that, inside A.re, we can refer to B instead
   of Foo__B thanks to the pre-opened Foo.ml/Foo_Foo.ml. But when these files are used by other libraries (which aren't
   compiled with foo.re pre-opened of course), they won't see module A or C, only Foo__A and Foo_C, aka in
   practice, they simply won't see them. This effectively means we've implemented namespacing!

   Note that we're generating a ml file rather than re, because this rebel theoretically works on pure
   ocaml projects too, with no dep on reason. */
let moduleAliasLibScheme
    buildDir::buildDir
    libRoot::libRoot
    libName::libName
    target::target
    sourcePaths::sourcePaths => {
  let {compiler, cmox} = target;
  let moduleAliasPath ext => moduleAliasFile buildDir::buildDir libName::libName ext::ext;
  let sourcePath = moduleAliasPath ".ml";

  /** Generates module A = Foo__A for the path src/.../A.re */
  let moduleAliasCode path => {
    let moduleName = tsm (pathToModule path);
    let moduleAliasedName = namespacedName libName::libName path::path;
    Printf.sprintf "module %s = %s\n" moduleName moduleAliasedName
  };

  /** We omit interface to create the alias file **/
  let sourceNotInterfacePaths = List.filter sourcePaths f::isImplementation;
  let fileContent = List.map sourceNotInterfacePaths f::moduleAliasCode |> String.concat sep::"";

  /** We suppress a few warnings here through -w.
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
  let action =
    bashf
      "ocamlfind %s -bin-annot -g -no-alias-deps -w -49 -w -30 -w -40 %s -c -impl %s -o %s 2>&1| berror; (exit ${PIPESTATUS[0]})"
      compiler
      target.flags.compile
      (tsp sourcePath)
      (tsp (moduleAliasPath cmox));

  /** TODO: do we even need the cmo file here? */
  let targets = List.map f::moduleAliasPath [cmox, ".cmi", ".cmt"];
  let compileRule = Rule.simple targets::targets deps::[Dep.path sourcePath] action::action;
  let contentRule =
    Rule.create targets::[sourcePath] (Dep.return (Action.save fileContent target::sourcePath));
  Scheme.rules [contentRule, compileRule]
};

/* We compile each file in the current library (say, foo). If a file's Bar.re, it'll be compiled to
   foo__Bar.{cmi, cmo, cmt}. As to why we're namespacing compiled outputs like this, see
   `moduleAliasFileScheme`. */
let compileSourcesScheme
    libRoot::libRoot
    target::target
    buildDir::buildDir
    libName::libName
    sourcePaths::sourcePaths => {
  let buildDirRoot = Path.dirname buildDir;
  let {compiler, cmox, cmax} = target;

  /** This is the module alias file generated through `moduleAliasFileScheme`, that we said
      we're gonna `-open` during `ocamlc` */
  let moduleAliasPath ext => moduleAliasFile buildDir::buildDir libName::libName ext::ext;
  let moduleName = Path.basename @@ moduleAliasPath "";

  /** Declare Module Alias Artficats as Deps for lib sources */
  let moduleAliasDep = Dep.all_unit (
    List.map [cmox, ".cmi", ".cmt", ".ml"] f::(fun ext => ext |> moduleAliasPath |> Dep.path)
  );

  /** Get npm dependencies and ocamlfind dependencies from the package's package.json.
      These are just possibles dependencies. We compute them so that we have a way
      segregate the deps that ocamldep oututs  */
  let thirdPartyNpmLibs = NpmDep.getThirdPartyNpmLibs libDir::libRoot;
  let ocamlfindPkgs = NpmDep.getThirdPartyOcamlfindLibs libDir::libRoot;

  /** Compile each file with all it's dependecies */
  let compileEachSourcePath path::path (firstPartyDeps, npmPkgs) => {
    let isInterface' = isInterface path;
    let hasInterface' = hasInterface sourcePaths::sourcePaths path;

    /** Helper fn to generate build dir paths for the current file artifacts **/
    let namespacedPath ext => rel dir::buildDir (namespacedName libName::libName path::path ^ ext);

    /** Need to include all the third party build directories for cmo artifacts. We compute these
        parts from the source paths instead of _build directory paths because on a fresh these directories
        will not be present in _build. Hence the indirect route. */
    let thirdPartyNpmDirPaths =
      List.map npmPkgs f::(fun libName => rel dir::buildDirRoot (tsl libName));

    /** flag to include all the dependencies build dir's */
    let includeDir =
      List.map thirdPartyNpmDirPaths f::(fun path => "-I " ^ tsp path) |> String.concat sep::" ";

    /** Include all ocamlfind dependencies under -package flag */
    let ocamlfindPackagesStr =
      switch ocamlfindPkgs {
      | [] => ""
      | _ => "-package " ^ (ocamlfindPkgs |> List.map f::tsl |> String.concat sep::",")
      };

    let extraFlags = target.flags.compile;

    /** Debug Info */
    /* print_endline ("Path: " ^ tsp path);
       print_endline "First Party Deps: ";
       print_endline ("Had Interface: " ^ string_of_bool hasInterface');
       print_endline ("Build Dir: " ^ tsp buildDir);
       print_endline ("Lib Dir: " ^ tsp libDir); */

    /** Rule for compiling .re/rei/ml/mli which generetes .cmi, .cmt, .cmo/.cmx dependind on
        the compiler

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
            "ocamlfind %s -pp refmt -g -w -30 -w -40 %s -I %s %s %s %s -o %s -c -intf %s 2>&1| berror; (exit ${PIPESTATUS[0]})"
          } else if (
            hasInterface' && String.is_suffix (Path.basename path) suffix::".re"
          ) {
            "ocamlfind %s -pp refmt -bin-annot -g -w -30 -w -40 %s -I %s %s %s %s -o %s -c -intf-suffix .rei -impl %s 2>&1| berror; (exit ${PIPESTATUS[0]})"
          } else {
            "ocamlfind %s -pp refmt -bin-annot -g -w -30 -w -40 %s -I %s %s %s %s -o %s -c -impl %s 2>&1| berror; (exit ${PIPESTATUS[0]})"
          }
        )
        compiler
        ("-open " ^ moduleName)
        (tsp buildDir)
        extraFlags
        ocamlfindPackagesStr
        includeDir
        (tsp (namespacedPath ""))
        (tsp path);

    /** In the case of byte/jsoo we are only depending on cmis. If the interface signature
        doesn't change, ocamlc doesn't need to recompile the dependent modules. But the same
        is not true for ocamlopt though due inter module optimizations. More here
        http://stackoverflow.com/questions/9843378/ocaml-module-types-and-separate-compilation */
    let firstPartyArtifactDeps =
      sourcePaths |>
      List.filter f::(fun path => List.exists firstPartyDeps f::(fun m => m == pathToModule path)) |>
      List.map
        f::(
          fun path =>
            [relD dir::buildDir (namespacedName libName::libName path::path ^ ".cmi")] @ (
              target.engine == "native" ?
                [relD dir::buildDir (namespacedName libName::libName path::path ^ cmox)] : []
            )
        ) |>
      List.fold init::[] f::(@);
    /* If the source file has an interface; include our own cmi as a dependency (our interface
       file should be compile before ourselves). */
    let firstPartyArtifactDeps =
      if (not isInterface' && hasInterface') {
        [
          relD dir::buildDir (namespacedName libName::libName path::path ^ ".cmi"),
          ...firstPartyArtifactDeps
        ]
      } else {
        firstPartyArtifactDeps
      };

    /** Deps here trigger the compilation of thirdParty npm packages and it is enough
        just specify the thirdParty's top level cma artifact */
    let thirdPartiesCmasDep =
      List.map
        npmPkgs
        f::(
          fun libName => {
            let buildDir = rel dir::buildDirRoot (tsl libName);
            Dep.path (moduleAliasFile buildDir::buildDir libName::libName ext::cmax)
          }
        ) |> Dep.all_unit;

    /** The overall dependencies include the js artifacts of the both self and third party
        and interface artifact if an interface exits */
    let deps = [Dep.path path, moduleAliasDep, thirdPartiesCmasDep, ...firstPartyArtifactDeps];

    /** Compute the artifacts extensions generate for each file type and then generate the
        correct build dir path for them */
    let targets = {
      let extns =
        if isInterface' {
          [".cmi"]
        } else if hasInterface' {
          [cmox, ".cmt"] @ (target.engine == "native" ? [".o"] : [])
        } else {
          [cmox, ".cmi", ".cmt"] @ (target.engine == "native" ? [".o"] : [])
        };
      List.map f::namespacedPath extns
    };
    Rule.simple targets::targets deps::deps action::action
  };

  /** Compute build graph (targets, dependencies) for the current path */
  let compileEachSourcePath' path =>
    OcamlDep.ocamlDepSource
      sourcePath::path paths::sourcePaths npmPkgs::thirdPartyNpmLibs target::target |>
    mapD (compileEachSourcePath path::path);
  Scheme.rules_dep (Dep.all (List.map sourcePaths f::compileEachSourcePath'))
};

/* This function assumes we're not using it at the top level src directory.
   Cma is a library file for the current library which bundles up all the lib's first-party compiled sources.
   This way, it's much easier, at the end, at the top level, to include each library's cma file to compile the
   final executable, than to tediously pass every single source file from every lib in order.

   There's a caveat though. We said we're only bundling up the current lib's first-party code; We _could_ have
   bundled up its third-party deps' cma too, but then we might get into duplicate artifact problem caused by
   e.g. library A and B both requiring and bundling C. So we can only bundle first-party code, and then, at
   the top, take all the transitive dependencies (libraries) cmas, figure out their relative order, and pass
   them in that order to the ocamlc command (this logic is in `finalOutputsScheme` below). Still tedious, but
   at least we're not passing individual source files in order. */
let compileCmaScheme
    sortedSourcePaths::sortedSourcePaths
    libName::libName
    buildDir::buildDir
    target::target => {
  let {compiler, cmox, cmax} = target;
  let moduleAliasPath ext => moduleAliasFile buildDir::buildDir libName::libName ext::ext;
  let moduleName = tsm (libToModule libName);
  let (moduleAliasCmaPath, moduleAliasCmoPath) = (moduleAliasPath cmax, moduleAliasPath cmox);

  /** All the cmos artifacts from the files in the libDir. To compile one cma file, we need
      to pass the compiled first-party sources in order to ocamlc */
  let cmos =
    List.map
      sortedSourcePaths
      f::(fun path => rel dir::buildDir (namespacedName libName::libName path::path ^ cmox));
  let cmosString = List.map cmos f::tsp |> String.concat sep::" ";

  /** TODO do we really need cmi files as deps here */
  let cmis =
    List.map
      sortedSourcePaths
      f::(fun path => rel dir::buildDir (namespacedName libName::libName path::path ^ ".cmi"));

  /** Flags:
      -open: compile the file as if [file being opened] was opened at the top of the file. In
      our case, we open our module alias file generated with `moduleAliasFileScheme`. See that
      function for more comment.

      -a: flag for building a library.

      -o: output file name.

      Example command: ocamlc -g -open Foo -a -o lib.cma foo.cmo aDependsOnMe.cmo a.cmo b.cmo */
  let action =
    bashf
      "%s -g -open %s -a -o %s %s %s 2>&1| berror; (exit ${PIPESTATUS[0]})"
      compiler
      moduleName
      (tsp moduleAliasCmaPath)
      (tsp moduleAliasCmoPath)
      cmosString;

  /** Final bundling. Time to get all the transitive dependencies... */
  Scheme.rules [
    Rule.simple
      targets::[moduleAliasCmaPath]
      deps::(List.map ([moduleAliasCmoPath] @ cmis @ cmos) f::Dep.path)
      action::action
  ]
};

let finalOutputsScheme
    buildDir::buildDir
    libName::libName
    target::target
    sortedSourcePaths::sortedSourcePaths => {
  let buildDirRoot = Path.dirname buildDir;
  let {compiler, cmox, cmax} = target;

  /** final compilation targets */
  let binaryOutput =
    target.engine == "native" ? rel dir::buildDir "app.native" : rel dir::buildDir "app.byte";
  let jsOutput = rel dir::buildDir "app.js";

  /** The top level module alias .cmo/cmx artifact path   */
  let moduleAliasCmoxPath = rel dir::buildDir (tsm (libToModule libName) ^ cmox);

  /** All the cmo/cmss artifacts from the files in the toplevel src dir **/
  /* To compile one cma file, we need to pass the compiled first-party sources in order to ocamlc */
  let cmoxArtifact path => rel dir::buildDir (namespacedName libName::libName path::path ^ cmox);
  let objectArtifact path => rel dir::buildDir (namespacedName libName::libName path::path ^ ".o");
  let cmoxArtifacts = List.map sortedSourcePaths f::cmoxArtifact;
  let objectArtifacts = List.map sortedSourcePaths f::objectArtifact;
  let cmoxsString = List.map cmoxArtifacts f::tsp |> String.concat sep::" ";

  /**  */
  let finalCompilation (npmPkgs, ocamlfindPkgs) => {

    /** Gather all the cma artifacts compiled from npm package **/
    let libraryCmax libName =>
      rel dir::(rel dir::buildDirRoot (tsl libName)) (tsm (libToModule libName) ^ cmax);
    let transitiveCmaxs = List.map npmPkgs f::libraryCmax;

    /** Include all ocamlfind dependencies under -package flag */
    let ocamlfindPackagesStr =
      ocamlfindPkgs != [] ?
        "-linkpkg -package " ^ (List.map ocamlfindPkgs f::tsl |> String.concat sep::",") : "";

    let extraFlags = target.flags.link;

    /** For ease of coding, we'll blindly include js_of_ocaml in the -I search path here, in case
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
    let action =
      bashf
        "ocamlfind %s %s %s -g -open %s -o %s %s %s %s 2>&1| berror; (exit ${PIPESTATUS[0]})"
        compiler
        extraFlags
        ocamlfindPackagesStr
        (tsm (libToModule libName))
        (tsp binaryOutput)
        (transitiveCmaxs |> List.map f::tsp |> String.concat sep::" ")
        (tsp moduleAliasCmoxPath)
        cmoxsString;

    /** TODO: I don't think cmis and cmts are being read here, so we don't need to include them. */
    let deps =
      (target.engine == "native" ? objectArtifacts : []) @ cmoxArtifacts @ transitiveCmaxs |>
      List.map f::Dep.path;
    let nativeRule = [Rule.simple targets::[binaryOutput] deps::deps action::action];

    /** generate app.js from app.byte */
    let jsooAction =
      bashf
        /* I don't know what the --linkall flag does, and does the --pretty flag work? Because the
           output is still butt ugly. Just kidding I love you guys. */
        "js_of_ocaml --source-map --no-inline --debug-info --pretty --linkall %s -o %s %s"
        target.flags.jsoo
        (tsp jsOutput)
        (tsp binaryOutput);
    let javascriptRule =
      target.engine == "jsoo" ?
        [Rule.simple targets::[jsOutput] deps::[Dep.path binaryOutput] action::jsooAction] : [];
    Scheme.rules (nativeRule @ javascriptRule)
  };

  /** Collect third party npm libraries and ocamlfind libraries that direct and transitive dependencies */
  OcamlDep.sortedTransitiveThirdPartyLibs paths::sortedSourcePaths target::target |>
  mapD finalCompilation |> Scheme.dep
};

let matchImplIntfFiles path implPaths::implPaths => {
  let sp = pathToModule path;
  implPaths |> List.map f::pathToModule |> List.exists f::(fun ep => ep == sp)
};

let compileLibScheme
    libRoot::libRoot
    libName::libName
    isTopLevelLib::isTopLevelLib
    buildDir::buildDir
    target::target => {
  let libDir = rel dir::libRoot "src";

  /** Construct schemes for modules alias, source files and cma artifact/final executable */
  let compileLibScheme' unsortedPaths => {
    let implPaths = List.filter unsortedPaths f::isImplementation;
    /* Passing impl paths because we only need the order of modules */
    OcamlDep.sortPathsTopologically paths::implPaths target::target |>
    mapD (
      fun sortedImplPaths => Scheme.all [
        moduleAliasLibScheme
          buildDir::buildDir
          libRoot::libRoot
          libName::libName
          target::target
          sourcePaths::unsortedPaths,
        compileSourcesScheme
          libRoot::libRoot
          buildDir::buildDir
          target::target
          libName::libName
          sourcePaths::unsortedPaths,
        isTopLevelLib ?
          /* if we're at the final, top level compilation, there's no need to build a cma output (and
             then generate an executable from it). We can cut straight to generating the executable. See
             `finalOutputsScheme`. */
          finalOutputsScheme
            buildDir::buildDir libName::libName target::target sortedSourcePaths::sortedImplPaths :
          compileCmaScheme
            buildDir::buildDir target::target libName::libName sortedSourcePaths::sortedImplPaths
      ]
    )
  };

  /** compute all the files in libDir recursively */
  let sourcePaths = getSourceFiles dir::libDir;
  /* If the current lib is toplevel lib, then we compute the paths that only dependencies of enty path */
  /* TODO entry paths are possibly sorted no need to sort them again. But it doesn't work fully */
  let entryPaths =
    isTopLevelLib ?
      {
        let entry = rel dir::Path.the_root target.entry;
        OcamlDep.entryPointDependencies entry::entry paths::sourcePaths target::target |>
        mapD (
          fun implPaths => List.filter sourcePaths f::(matchImplIntfFiles implPaths::implPaths)
        )
      } :
      Dep.all (sourcePaths |> List.map f::Dep.return);
  entryPaths |> bindD compileLibScheme' |> Scheme.dep
};

let ocamlTargets =
  rebelConfig.targets |>
  List.filter
    f::(
      fun target => target.engine == "native" || target.engine == "byte" || target.engine == "jsoo"
    );

let targetOutputFile target => {
  let buildDir = rel dir::(rel dir::build target.target) "src";
  /* TODO refactor with polymorphic variants */
  switch target.engine {
  | "native" => Dep.path (rel dir::buildDir "app.native")
  | "jsoo" => Dep.path (rel dir::buildDir "app.js")
  | _ => Dep.path (rel dir::buildDir "app.byte")
  }
};

let defaultPaths = List.map ocamlTargets f::targetOutputFile;

let scheme dir::dir =>
  if (dir == Path.the_root) {
    Scheme.rules [Rule.default dir::dir defaultPaths]
  } else if (
    Path.is_descendant dir::build dir
  ) {
    let targetName = extractTargetName dir::dir;
    let targetConfig = findTarget targetName;
    let packageName = extractPackageName dir::dir;
    let isTopLevelLib = packageName == "src";
    let libName = isTopLevelLib ? Lib (targetName ^ "_Tar") : Lib (Path.basename dir);
    let libRoot = isTopLevelLib ? Path.dirname topSrcDir : rel dir::nodeModulesRoot packageName;
    switch targetConfig.engine {
    | "jsoo"
    | "byte"
    | "native" =>
      compileLibScheme
        libRoot::libRoot
        libName::libName
        isTopLevelLib::isTopLevelLib
        buildDir::dir
        target::targetConfig
    | _ => Scheme.no_rules
    }
  } else {
    Scheme.no_rules
  };
