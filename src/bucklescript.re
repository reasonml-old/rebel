/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

let module Dep = Jenga_lib.Api.Dep;

let module Path = Jenga_lib.Api.Path;

let module Rule = Jenga_lib.Api.Rule;

let module Action = Jenga_lib.Api.Action;

let module Scheme = Jenga_lib.Api.Scheme;

open Utils;

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
    sourcePaths::sourcePaths
    libName::libName
    target::target => {
  let name ext => rel dir::buildDir (tsm (libToModule libName) ^ ext);
  let sourcePath = name ".ml";
  let targets = [".cmj", ".cmi", ".cmt"] |> List.map f::name;

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
      "bsc.exe -bin-annot -g %s -no-alias-deps -w -49 -w -30 -w -40 -c -impl %s -o %s 2>&1; (exit ${PIPESTATUS[0]})"
      target.flags.compile
      (Path.to_string sourcePath)
      (Path.to_string (name ".cmj"));

  /** TODO: do we even need the cmj file here? */
  let compileRule = Rule.simple targets::targets deps::[Dep.path sourcePath] action::action;
  let contentRule =
    Rule.create targets::[sourcePath] (Dep.return (Action.save fileContent target::sourcePath));
  Scheme.rules [contentRule, compileRule]
};

let compileSourcesScheme
    libRoot::libRoot
    buildDir::buildDir
    libName::libName
    target::target
    sourcePaths::sourcePaths => {
  let isTopLevelLib = libRoot == Path.the_root;
  let buildDirRoot = Path.dirname buildDir;

  /** Get npm dependencies and ocamlfind dependencies from the package's package.json.
      These are just possibles dependencies. We compute them so that we have a way
      segregate the deps that ocamldep oututs  */
  let thirdPartyNpmLibs = NpmDep.getThirdPartyNpmLibs libDir::libRoot;
  let ocamlfindPkgs = NpmDep.getThirdPartyOcamlfindLibs libDir::libRoot;

  /** Compute Module Alias dependencies for dependencies only */
  let moduleAliasDep = Dep.all_unit (
    ["cmi", "cmj", "cmt"] |>
    List.map f::(fun extension => relD dir::buildDir (tsm (libToModule libName) ^ "." ^ extension))
  );

  /** Compiling the current source file depends on all of the cmis of all its third-party libraries'
      source files being compiled. This is very coarse since in reality, we only depend on a few source
      files of these third-party libs. But ocamldep isn't granular enough to give us this information
      yet. */
  let computeNpmLibArtifacts libName => {
    /* if one of a third party library foo's source is Hi.re, then it resides in
       `node_modules/foo/src/Hi.re`, and its cmj artifacts in `_build/foo/Foo__Hi.cmj` */
    let thirdPartySrcPath = rel dir::(rel dir::nodeModulesRoot (tsl libName)) "src";
    let thirdPartyBuildPath path::path ext::ext =>
      relD
        dir::(rel dir::buildDirRoot (tsl libName))
        (namespacedName libName::libName path::path ^ ext);

    /** FIXME Temporary workaround for bucklescript bug */
    let bsThirdPartyBuildPath path::path ext::ext =>
      relD
        dir::(rel dir::buildDirRoot (tsl libName))
        (bsNamespacedName libName::libName path::path ^ ext);

    /** No need to glob `.rei/.mli`s here. We're only getting the file names to
        construct cmj paths. We depend on cmj artifacts rather cmi artifacts because
        cmi articfacts can be just generated with interface files and it is not sufficient
        to build the target */
    /* FIXME the js dependencies are Temporary and can just be cmj dependencies once
       bucklescript bug is removed */
    getSourceFiles dir::thirdPartySrcPath |> (
      fun thirdPartySources =>
        List.map thirdPartySources f::(fun path => thirdPartyBuildPath path::path ext::".cmj") @
        List.map thirdPartySources f::(fun path => bsThirdPartyBuildPath path::path ext::".js")
    ) |> Dep.all_unit
  };

  /** Compile each file with all it's dependecies */
  let compilePathScheme path::path (firstPartyDeps, npmPkgs) => {
    let isInterface' = isInterface path;
    let hasInterface' = hasInterface sourcePaths::sourcePaths path;

    /** Helper functions to generate build dir paths **/
    let namespacedPath ext => rel dir::buildDir (namespacedName libName::libName path::path ^ ext);

    /** flag to include all the dependencies build dir's **/
    let includeDir =
      npmPkgs |> List.map f::(fun libName => "-I " ^ tsp (rel dir::buildDirRoot (tsl libName))) |>
      String.concat sep::" ";

    /** Flag for including ocamlfind packages */
    let ocamlfindPackagesStr =
      switch ocamlfindPkgs {
      | [] => ""
      | libs => "-bs-package-include " ^ (libs |> List.map f::tsl |> String.concat sep::",")
      };

    /** Debug Info */
    /* print_endline ("Path: " ^ tsp path);
       print_endline "First Party Deps: ";
       print_endline ("Had Interface: " ^ string_of_bool hasInterface');
       print_endline ("Build Dir: " ^ tsp buildDir);
       print_endline ("Lib Dir: " ^ tsp libDir); */

    /** Rule for compiling .re/rei/ml/mli to .js

        More Flags:
        -bs-package-output  set npm-output-path: [opt_module]:path, for example: 'lib/cjs', 'amdjs:lib/amdjs' and 'goog:lib/gjs'

        -bs-package-name is set `self` to that it produces right require calls. */
    let action =
      bashf
        (
          if isInterface' {
            "bsc.exe -g %s %s -pp refmt -bs-package-name self -bs-package-output commonjs:%s %s %s -o %s -c -intf %s 2>&1; (exit ${PIPESTATUS[0]})"
          } else if (
            hasInterface' && String.is_suffix (Path.basename path) suffix::".re"
          ) {
            "bsc.exe -g %s %s -pp refmt -bin-annot -bs-package-name self -bs-package-output commonjs:%s %s %s -o %s -c -intf-suffix .rei -impl %s 2>&1; (exit ${PIPESTATUS[0]})"
          } else {
            "bsc.exe -g %s %s -pp refmt -bin-annot -bs-package-name self -bs-package-output commonjs:%s %s %s -o %s -c -impl %s 2>&1; (exit ${PIPESTATUS[0]})"
          }
        )
        target.flags.compile
        ocamlfindPackagesStr
        (tsp buildDir)
        ("-open " ^ tsm (libToModule libName))
        (includeDir ^ " -I " ^ tsp buildDir)
        (tsp (namespacedPath ""))
        (tsp path);

    /** Compute the artifacts extensions generate for each file type and then generate the
        correct build dir path for them

        FIXME Due to BuckleScript bug #757, we around it by using simplePath **/
    let targets =
      if isInterface' {
        [namespacedPath ".cmi"]
      } else {
        let extns =
          if hasInterface' {
            [".cmj", ".cmt", ".js"]
          } else {
            [".cmi", ".cmj", ".cmt", ".js"]
          };
        List.map f::namespacedPath extns
      };

    /** FIXME is this comment valid?
        compiling here only needs cmjs. If the interface signature doesn't change, ocaml doesn't need
        to recompile the dependent modules. Win. */
    let firstPartyArtifacts =
      sourcePaths |>
      List.filter f::(fun path => List.exists firstPartyDeps f::(fun m => m == pathToModule path)) |>
      List.map
        f::(
          fun path => [
            relD dir::buildDir (namespacedName libName::libName path::path ^ ".cmj"),
            relD dir::buildDir (bsNamespacedName libName::libName path::path ^ ".js")
          ]
        ) |>
      List.fold init::[] f::(@);
    let firstPartyArtifacts =
      if hasInterface' {
        [
          /* We're a source file with an interface; include our own cmi as a dependency (our interface
             file should compile before ourselves). */
          relD dir::buildDir (namespacedName libName::libName path::path ^ ".cmi"),
          ...firstPartyArtifacts
        ]
      } else {
        firstPartyArtifacts
      };
    let thirdPartyArtifacts = List.map npmPkgs f::computeNpmLibArtifacts |> Dep.all_unit;

    /** The overall dependecies include the cmj artifacts of the both self and third party
        and interface artifact if an interface exits **/
    let deps = [Dep.path path, thirdPartyArtifacts, moduleAliasDep, ...firstPartyArtifacts];

    /** Workaround for BuckleScript bug https://github.com/bloomberg/bucklescript/issues/757
        Action to copy the file to match require call */
    let copyTarget = rel dir::buildDir (bsNamespacedName libName::libName path::path ^ ".js");
    let copyAction = bashf "cp %s %s" (tsp (namespacedPath ".js")) (tsp copyTarget);

    /** We perform the copy only Linux because file are case-insensitive on OSX  */
    let copyRule =
      switch os_name {
      | "Linux" => [
          Rule.simple
            targets::[copyTarget] deps::[Dep.path (namespacedPath ".js")] action::copyAction
        ]
      | _ => []
      };
    let bucklescriptTargets =
      rebelConfig.targets |> List.filter f::(fun t => t.engine == "bucklescript");

    /** All after all the compilation with bsc finishes. Duplicate the final target
        file with entry path name */
    let copyTargetRule target => {
      let source = target.entry |> rel dir::Path.the_root;
      let buildDir = rel dir::(rel dir::build target.target) (tsp topSrcDir);
      let copyDep =
        rel
          dir::buildDir
          (bsNamespacedName libName::(Lib (target.target ^ "_Tar")) path::source ^ ".js");
      let copyTarget = rel dir::buildDir ((source |> fileNameNoExtNoDir) ^ ".js");
      let copyAction = bashf "cp %s %s" (tsp copyDep) (tsp copyTarget);
      Rule.simple targets::[copyTarget] deps::[Dep.path copyDep] action::copyAction
    };
    let copyTargetRules = isTopLevelLib ? List.map bucklescriptTargets f::copyTargetRule : [];

    /** Compile JS from BuckleScript and copy the file to match require call */
    Scheme.rules (
      [Rule.simple targets::targets deps::deps action::action] @ copyRule @ copyTargetRules
    )
  };

  /** Compute build graph (targets, dependencies) for the current path */
  let compilePathScheme' path =>
    OcamlDep.ocamlDepSource
      sourcePath::path paths::sourcePaths npmPkgs::thirdPartyNpmLibs target::target |>
    mapD (compilePathScheme path::path);
  Scheme.all (List.map sourcePaths f::(fun path => compilePathScheme' path |> Scheme.dep))
};

let matchImplIntfFiles path implPaths::implPaths => {
  let sp = pathToModule path;
  implPaths |> List.map f::pathToModule |> List.exists f::(fun ep => ep == sp)
};

let compileLibScheme
    libName::libName
    isTopLevelLib::isTopLevelLib
    libRoot::libRoot
    target::target
    buildDir::buildDir => {
  let libDir = rel dir::libRoot "src";

  /** Construct schemes for modules alias, source files  */
  let compileLibScheme' unsortedPaths => Scheme.all [
    moduleAliasFileScheme
      buildDir::buildDir sourcePaths::unsortedPaths libName::libName target::target,
    compileSourcesScheme
      libRoot::libRoot
      buildDir::buildDir
      libName::libName
      target::target
      sourcePaths::unsortedPaths
  ];

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
  entryPaths |> mapD compileLibScheme' |> Scheme.dep
};

let bucklescriptTargets =
  rebelConfig.targets |> List.filter f::(fun t => t.engine == "bucklescript");

let targetOutputFile target => {
  let fileName = target.entry |> rel dir::Path.the_root |> fileNameNoExtNoDir;
  let buildDir = rel dir::(rel dir::build target.target) (tsp topSrcDir);
  relD dir::buildDir (fileName ^ ".js")
};

let defaultPaths = List.map bucklescriptTargets f::targetOutputFile;

let scheme dir::dir =>
  if (dir == Path.the_root) {
    Scheme.all [Scheme.rules [Rule.default dir::dir defaultPaths]]
  } else if (
    Path.is_descendant dir::build dir
  ) {
    let dirName = Path.basename dir;
    let targetName = extractTargetName dir::dir;
    let targetConfig = findTarget targetName;
    let isTopLevelLib = Path.basename dir == "src";
    let libName = isTopLevelLib ? Lib (targetName ^ "_Tar") : Lib (Path.basename dir);
    let libRoot = isTopLevelLib ? Path.the_root : rel dir::nodeModulesRoot dirName;
    switch targetConfig.engine {
    | "bucklescript" =>
      compileLibScheme
        libRoot::libRoot
        isTopLevelLib::isTopLevelLib
        libName::libName
        buildDir::dir
        target::targetConfig
    | _ => Scheme.no_rules
    }
  } else {
    Scheme.no_rules
  };
