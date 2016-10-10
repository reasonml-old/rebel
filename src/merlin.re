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

let module Run = Jenga_lib.Run;

let module Env = Jenga_lib.Api.Env;

open Utils;

/* See comment in the `sprintf` */
let dotMerlinScheme
    isTopLevelLib::isTopLevelLib
    libName::libName
    dir::dir
    bscBackend::bscBackend=true => {
  let dotMerlinPath = rel dir::dir ".merlin";
  let thirdPartyNpmLibs = NpmDep.getThirdPartyNpmLibs libDir::dir;

  /** Bucklescript Specific stuff **/
  /* TODO change absolute path to relative path once merlin supports relative paths for flags */
  let bsBuildArtifactsPath =
    Path.reach_from dir::dir (rel dir::nodeModulesRoot "bs-platform/lib/ocaml");
  let bucklescriptBuildArtifacts =
    bscBackend ? "# Bucklescript build artifacts\nB " ^ bsBuildArtifactsPath : "";
  let merlinWorkAroundComment =
    "# Currently the we use absolute path instead of relative path for bsppx.exe\n" ^ "# due to bug in merlin but this will be fixed in future.";
  let bsppxAbsolutePath = Path.to_absolute_string (
    rel dir::Path.the_root "node_modules/bs-platform/bin/bsppx.exe"
  );

  /** Only include thirdPartyNpmLibs whose sources are rebel compatible **/
  let thirdPartyNpmMerlinSources =
    thirdPartyNpmLibs |>
    List.map
      f::(
        fun libName =>
          "S " ^ Path.reach_from dir::dir (rel dir::(rel dir::nodeModulesRoot (tsl libName)) "src")
      ) |>
    String.concat sep::"\n";
  let thirdPartyNpmMerlinDeps =
    thirdPartyNpmLibs |>
    List.map f::(fun libName => relD dir::(rel dir::nodeModulesRoot (tsl libName)) ".merlin");
  let ocamlfindPkgs =
    switch (NpmDep.getThirdPartyOcamlfindLibs libDir::dir) {
    | [] => ""
    | _ as ls => "PKG " ^ (ls |> List.map f::tsl |> String.concat sep::" ")
    };

  /** Overwrites existing conf   **/
  let saveMerlinAction previousContents::previousContents => {

    /** Read the existing custom config **/
    let customConfig =
      switch previousContents {
      | "" => ""
      | _ =>
        String.split_lines previousContents |>
        List.drop_while f::(fun s => s != "# User Custom config here") |>
        List.drop_while f::(fun s => s == "# User Custom config here") |>
        String.concat sep::"\n"
      };
    let dotMerlinContent =
      Printf.sprintf
        {| # DO NOT EDIT THIS SECTION. CUSTOM CONFIG GOES BELOW
# This file is autogenerated for
# [Merlin](https://github.com/the-lambda-church/merlin), a static analyser for
# OCaml that provides autocompletion, jump-to-location, recoverable syntax
# errors, type errors detection, etc., that your editor can use. To activate it,
# one usually provides a .merlin file at the Path.the_root of a project, describing where
# the sources and artifacts are. Since we dictated the project structure, we can
# auto generate .merlin files!

# S is the merlin flag for source files
%s

# Include all the third-party sources too. You might notice that we've put a
# .merlin into each node_modules package. This is subtle; in short, it's to make
# jump-to-location work correctly in conjunction with our build & namespacing
# setup, when you jump into a third-party file.
%s

# B stands for build (artifacts). We generate ours into _build
B %s

%s

# PKG lists packages found through ocamlfind (aka findlib), a tool for finding
# the location of third-party dependencies. For us, most of our third-party deps
# reside in `node_modules/` (made visible to Merlin through the S command
# above); this PKG command is for discovering the opam/ocamlfind packages.
%s

# FLG is the set of flags to pass to Merlin, as if it used ocamlc to compile and
# understand our sources. You don't have to understand what these flags are for
# now; but if you're curious, go check the rebel.ml that generated this
# .merlin at https://github.com/reasonml/rebel
FLG -w -30 -w -40 %s

%s
%s

# User Custom config here
%s
|}
        (isTopLevelLib ? "S src" : "")
        thirdPartyNpmMerlinSources
        (Path.reach_from dir::dir (rel dir::buildDirRoot "*"))
        bucklescriptBuildArtifacts
        ocamlfindPkgs
        (isTopLevelLib && bscBackend ? "" : "-open " ^ tsm (libToModule libName))
        (bscBackend ? merlinWorkAroundComment : "")
        (bscBackend ? "FLG -ppx " ^ bsppxAbsolutePath : "")
        customConfig;
    Action.save dotMerlinContent target::dotMerlinPath
  };
  Scheme.rules [
    Rule.simple
      targets::[dotMerlinPath]
      deps::thirdPartyNpmMerlinDeps
      action::(saveMerlinAction previousContents::(readFile path::dotMerlinPath))
  ]
};

let scheme dir::dir =>
  /* We generate many .merlin files, one per third-party library (and on at the top). Additionally, this is
     the only case where we generate some artifacts outside of _build/. Most of this is so that Merlin's
     jump-to-location could work correctly when we jump into a third-party source file. As to why exactly we
     generate .merlin with the content that it is, call 1-800-chenglou-plz-help. */
  if (dir == Path.the_root) {
    let toplevelScheme =
      dotMerlinScheme
        isTopLevelLib::true
        dir::dir
        libName::topLibName
        bscBackend::(rebelConfig.backend == "bucklescript");
    Scheme.all [
      Scheme.rules [Rule.default dir::dir [relD dir::Path.the_root ".merlin"]],
      toplevelScheme
    ]
  } else if (
    Path.dirname dir == nodeModulesRoot
  ) {
    let libName = Lib (Path.basename dir);
    dotMerlinScheme
      isTopLevelLib::false
      dir::dir
      libName::libName
      bscBackend::(rebelConfig.backend == "bucklescript")
  } else {
    Scheme.no_rules
  };
