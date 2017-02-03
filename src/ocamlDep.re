/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

let module Dep = Jenga_lib.Api.Dep;

let module Path = Jenga_lib.Api.Path;

let module Action = Jenga_lib.Api.Action;

open Utils;

/* Wrapper for the CLI `ocamldep`. Take the output, process it a bit, and pretend we've just called a regular
   ocamldep OCaml function. Note: the `ocamldep` utility doesn't give us enough info for fine, accurate module
   tracking in the presence of `open` */
let ocamlDep source::source target::target => {
  let flag = isInterface source ? "-intf" : "-impl";
  let ppx = target.engine == "bucklescript" ? "-ppx bsppx.exe" : "";
  let extraFlags = target.flags.dep;
  let envvars = target.flags.envvars;
  /* betterError doesn't support bucklescript yet */
  let berror = target.engine == "bucklescript" ? "" : "2>&1| berror";

  /** seems like refmt intelligently detects source code type (re/ml) */
  let getDepAction () =>
    bashf
      "%s ocamlfind ocamldep -pp 'refmt --print binary' %s %s -ml-synonym .re -mli-synonym .rei -modules -one-line %s %s %s; (exit ${PIPESTATUS[0]})"
      envvars
      ppx
      extraFlags
      flag
      (tsp source)
      berror;
  let action = Dep.action_stdout (Dep.path source |> mapD getDepAction);
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
let sourceSelfDependencies source::source paths::paths target::target =>
  ocamlDep source::source target::target |>
  mapD (
    fun (source, deps) => {
      let originalModule = pathToModule source;
      /* Filter  foo.rei */
      let paths = List.filter paths f::isImplementation;
      /* If the current file's Foo.re, and it depend on Foo, then it's certainly not depending on
         itself, which means that Foo either comes from a third-party module (which we can ignore
         here), or is a nested module from an `open`ed module, which ocamldep would have detected and
         returned in this list. */
      let deps = List.filter deps f::(fun m => m != originalModule);
      List.filter paths (fun p => List.exists deps f::(fun m => m == pathToModule p))
    }
  );

/* Same as sourceSelfDependencies but return modules instead paths  */
let sourceSelfModDeps source::source paths::paths target::target =>
  sourceSelfDependencies source::source paths::paths target::target |>
  mapD (fun paths => List.map paths f::pathToModule);

/*  */
let entryPointDependencies entry::entry paths::paths target::target => {
  let rec computeDeps acc::acc sourcePaths::sourcePaths =>
    switch sourcePaths {
    | [] => acc
    | [source, ...rest] =>
      let computeNext entryDeps =>
        List.exists entryDeps f::(fun p => p == source) ?
          computeDeps acc::acc sourcePaths::rest :
          sourceSelfDependencies source::source paths::paths target::target |>
          bindD (
            fun deps =>
              computeDeps acc::(Dep.return (entryDeps @ [source])) sourcePaths::(rest @ deps)
          );
      Dep.bind acc computeNext
    };
  sourceSelfDependencies source::entry paths::paths target::target |>
  bindD (fun deps => computeDeps acc::(Dep.return [entry]) sourcePaths::deps)
};

/* Get only the dependencies on sources in the current library. */
let ocamlDepSource sourcePath::sourcePath paths::paths npmPkgs::npmPkgs target::target => {
  let segregateDeps (source, deps) => {
    let originalModule = pathToModule source;

    /** Dedupe, because we might have foo.re and foo.rei */
    let sourceModules = List.map paths f::pathToModule |> List.dedup;
    /*TODO convert to List.Assoc.t  */
    let npmPkgsModules = List.map npmPkgs f::libToModule;

    /** If the current file's Foo.re, and it depend on Foo, then it's certainly not depending on
        itself, which means that Foo either comes from a third-party module (which we can ignore
        here), or is a nested module from an `open`ed module, which ocamldep would have detected and
        returned in this list. */
    let segregateDependencies (firstPartyDeps, npmPkgs') m => {
      let moduleExists = List.exists f::(fun m' => m == m');
      if (moduleExists sourceModules) {
        (firstPartyDeps @ [m], npmPkgs')
      } else if (
        moduleExists npmPkgsModules
      ) {
        let pos = List.foldi init::0 f::(fun i acc m' => m' == m ? i : acc) npmPkgsModules;
        (firstPartyDeps, npmPkgs' @ [List.nth_exn npmPkgs pos])
      } else {
        (firstPartyDeps, npmPkgs')
      }
    };
    List.filter deps f::(fun m => m != originalModule) |>
    List.fold init::([], []) f::segregateDependencies
  };

  /** Compute ocamldep on source file which gives all the top level modules in the file.
      Now segregate them into firstPartyDeps, npmPkgs, ocamlfindPkgs  */
  ocamlDep source::sourcePath target::target |> mapD segregateDeps
};

/* Compute both third party npm and ocamlfind libs for library. We specify for paths as variable
   because for top level lib we pass entry point paths as paths */
let ocamlDepThirdPartyLib paths::paths libDir::libDir target::target => {
  let npmPkgs = NpmDep.getThirdPartyNpmLibs libDir::libDir;
  let ocamlfindPkgs = NpmDep.getThirdPartyOcamlfindLibs libDir::libDir;
  List.map
    paths
    f::(
      fun source => ocamlDepSource sourcePath::source paths::paths npmPkgs::npmPkgs target::target
    ) |> Dep.all |>
  mapD (fun ls => List.fold init::[] ls f::(fun n' (_, n) => n' @ n)) |>
  mapD (fun npmPkgs => (List.dedup npmPkgs, ocamlfindPkgs))
};

let sortedTransitiveThirdPartyLibs paths::paths target::target => {
  let rec computeAllDeps acc::acc npmPkgs::npmPkgs =>
    switch npmPkgs {
    | [] => acc
    | [pkg, ...rest] =>
      let libDir = rel dir::nodeModulesRoot (tsl pkg);
      let paths = getSourceFiles dir::(rel dir::libDir "src");
      acc |>
      bindD (
        fun (npmAcc, ocamlAcc) =>
          List.exists npmAcc f::(fun (p, _) => p == pkg) ?
            computeAllDeps acc::acc npmPkgs::rest :
            ocamlDepThirdPartyLib libDir::libDir paths::paths target::target |>
            bindD (
              fun (npmPkgs', ocamlfindPkgs') => {
                let acc' = Dep.return ([(pkg, npmPkgs'), ...npmAcc], ocamlAcc @ ocamlfindPkgs');
                computeAllDeps acc::acc' npmPkgs::(rest @ npmPkgs')
              }
            )
      )
    };
  ocamlDepThirdPartyLib libDir::Path.the_root paths::paths target::target |>
  bindD (
    fun (npmPkgs, ocamlfindPkgs) =>
      computeAllDeps acc::(Dep.return ([], ocamlfindPkgs)) npmPkgs::npmPkgs
  ) |>
  mapD (fun (npmPkgs, ocamlfindPkgs) => (topologicalSort npmPkgs, List.dedup ocamlfindPkgs))
};

/* Used to compile a library file. The compile command requires files to be passed in order. If A requires B
   but B is passed after A in the command, the compilation will fail with e.g. "module B not found" when
   compiling A */
let sortPathsTopologically paths::paths target::target => {
  let pathsAsModulesOriginalCapitalization =
    List.map paths f::(fun path => (pathToModule path, path));
  let pathsAsModules = List.map pathsAsModulesOriginalCapitalization f::fst;
  let selfModuleDepsOfPath path => sourceSelfModDeps source::path paths::paths target::target;
  List.map paths f::selfModuleDepsOfPath |> Dep.all |>
  mapD (
    fun moduleDepsForPaths =>
      List.zip_exn pathsAsModules moduleDepsForPaths |> topologicalSort |>
      List.map f::(fun m => List.Assoc.find_exn pathsAsModulesOriginalCapitalization m)
  )
};
