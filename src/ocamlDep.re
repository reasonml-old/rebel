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
let ocamlDep source::source => {
  let flag = isInterface source ? "-intf" : "-impl";
  let ppx = rebelConfig.backend == "bucklescript" ? "-ppx bsppx.exe" : "";
  let berror = rebelConfig.backend == "bucklescript" ? "" : "| berror";
  /* seems like refmt intelligently detects source code type (re/ml) */
  let getDepAction () =>
    bashf
      "ocamldep -pp refmt %s -ml-synonym .re -mli-synonym .rei -modules -one-line %s %s 2>&1 %s; (exit ${PIPESTATUS[0]})"
      ppx
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
let ocamlDepCurrentSources sourcePath::sourcePath paths::paths =>
  ocamlDep source::sourcePath |>
  mapD (
    fun (source, deps) => {
      let originalModule = pathToModule source;
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

/* Get only the dependencies on sources in the current library. */
let ocamlDepSource
    sourcePath::sourcePath
    paths::paths
    npmPkgs::npmPkgs
    ocamlfindPkgs::ocamlfindPkgs =>
  ocamlDep source::sourcePath |>
  mapD (
    fun (source, deps) => {
      let originalModule = pathToModule source;

      /** Dedupe, because we might have foo.re and foo.rei */
      let sourceModules = List.map paths f::pathToModule |> List.dedup;
      let npmPkgsModules = List.map npmPkgs f::libToModule;
      let ocamlfindPkgsModules = List.map ocamlfindPkgs f::libToModule;

      /** If the current file's Foo.re, and it depend on Foo, then it's certainly not depending on
          itself, which means that Foo either comes from a third-party module (which we can ignore
          here), or is a nested module from an `open`ed module, which ocamldep would have detected and
          returned in this list. */
      List.filter deps f::(fun m => m != originalModule) |>
      List.fold
        init::([], [], [])
        f::(
          fun (firstParty, npmPkgs', ocamlfindPkgs') m =>
            if (List.exists sourceModules f::(fun m' => m == m')) {
              (firstParty @ [m], npmPkgs', ocamlfindPkgs')
            } else if (
              List.exists npmPkgsModules f::(fun m' => m == m')
            ) {
              let pos = List.foldi init::0 f::(fun i acc m' => m' == m ? acc + i : acc) npmPkgsModules;
              (firstParty, npmPkgs' @ [ List.nth_exn npmPkgs pos ], ocamlfindPkgs')
            } else if (
              List.exists ocamlfindPkgsModules f::(fun m' => m == m')
            ) {
              let pos = List.foldi init::0 f::(fun i acc m' => m' == m ? acc + i : acc) ocamlfindPkgsModules;
              (firstParty, npmPkgs', ocamlfindPkgs' @ [ List.nth_exn ocamlfindPkgs pos ])
            } else {
              (firstParty, npmPkgs', ocamlfindPkgs')
            }
        )
    }
  );

/* Used to compile a library file. The compile command requires files to be passed in order. If A requires B
   but B is passed after A in the command, the compilation will fail with e.g. "module B not found" when
   compiling A */
let sortPathsTopologically paths::paths => {
  let pathsAsModulesOriginalCapitalization =
    List.map paths f::(fun path => (pathToModule path, path));
  let pathsAsModules = List.map pathsAsModulesOriginalCapitalization f::fst;
  let moduleDepsForPathsD =
    paths |> List.map f::(fun path => ocamlDepCurrentSources sourcePath::path paths::paths) |> Dep.all;
  moduleDepsForPathsD |>
  mapD (
    fun moduleDepsForPaths =>
      List.zip_exn pathsAsModules moduleDepsForPaths |> topologicalSort |>
      List.map f::(fun m => List.Assoc.find_exn pathsAsModulesOriginalCapitalization m)
  )
};
