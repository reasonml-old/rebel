/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

open Yojson.Basic;

open Jenga_lib.Api;

open Utils;


/** Handling dependencies:

    In a reason/bucklescript project, there may be reason/bs dependencies or npm dependencies or both.
    We need to filter all npm dependencies from the build process. We will test if package is reason/ml
    dependencies if it has either rebel in the package.json field of the dependency. */
let isDirRebelCampatible libDir::libDir => {
  let packageJsonPath = Path.relative dir::libDir "package.json";
  [from_file (Path.to_string packageJsonPath)] |> Util.filter_member "rebel" |> List.length != 0
};

let withRebelCompatibleLibs =
  List.filter
    f::(fun lib => isDirRebelCampatible libDir::(Path.relative dir::nodeModulesRoot (tsl lib)));

/* Simply read into the package.json "dependencies" field. */
let getThirdPartyNpmLibs libDir::libDir => {
  let packageJsonPath = Path.relative dir::libDir "package.json";
  let deps =
    [from_file (Path.to_string packageJsonPath)] |> Util.filter_member "dependencies" |> Util.filter_assoc;
  let libs =
    switch deps {
    | [x] => x |> List.map f::fst |> List.map f::(fun name => Lib name)
    | _ => []
    };
  withRebelCompatibleLibs libs
};

/* Simply read into the package.json "rebel.ocamlfindDependencies" field. */
let getThirdPartyOcamlfindLibs libDir::libDir => {
  let packageJsonPath = Path.relative dir::libDir "package.json";
  let deps =
    from_file (Path.to_string packageJsonPath) |> Util.member "rebel" |>
    Util.to_option (fun a => a |> Util.member "ocamlfindDependencies");
  switch deps {
  /** TODO Remove object notation at a later point **/
  | Some (`Assoc d) => d |> List.map f::fst |> List.map f::(fun name => Lib name)
  | Some (`List d) => d |> List.map f::Util.to_string |> List.map f::(fun name => Lib name)
  | _ => []
  }
};

/* Figure out the order in which third-party libs should be compiled, based on their dependencies (the
   depended is compiled before the dependent). */
let sortedTransitiveThirdPartyNpmLibsIncludingSelf's npmPkgs::npmPkgs => {
  let thirdPartyDeps =
    List.map
      npmPkgs
      f::(
        fun name => {
          /* third party's own third party libs */
          let libDir = Path.relative dir::nodeModulesRoot (tsl name);
          (name, getThirdPartyNpmLibs libDir::libDir)
        }
      );
  /* `topologicalSort` will also return our own deps. */
  topologicalSort thirdPartyDeps
};

let transitiveThirdPartyOcamlfindLibsIncludingSelf's ocamlfindPkgs::ocamlfindPkgs npmPkgs::npmPkgs => {
  let thirdPartyLibDirs =
    List.map npmPkgs f::(fun name => Path.relative dir::nodeModulesRoot (tsl name));
  /* each third party's own third party libs */
  let thirdPartiesThirdPartyOcamlfindLibNamesD =
    List.map thirdPartyLibDirs f::(fun libDir => getThirdPartyOcamlfindLibs libDir::libDir);
  ocamlfindPkgs @ List.concat thirdPartiesThirdPartyOcamlfindLibNamesD |> List.dedup
};
