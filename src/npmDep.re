/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

open Yojson.Basic;

open Jenga_lib.Api;

/*
  Handling dependencies:

  In a reason/bucklescript project, there may be reason/bs dependencies or npm dependencies or both.
  We need to filter all npm dependencies from the build process. We will test if package is reason/ml
  dependencies if it has either rebel in the package.json field of the dependency.
*/

let isDirRebelCampatible libDir::libDir => {
  let packageJsonPath = Path.relative dir::libDir "package.json";
  [from_file (Path.to_string packageJsonPath)] |> Util.filter_member "rebel" |> List.length != 0
};

let withRebelCompatibleLibs =
  List.filter
    f::(
      fun lib =>
        isDirRebelCampatible libDir::(Path.relative dir::Utils.nodeModulesRoot (Utils.tsl lib))
    );

/* Simply read into the package.json "dependencies" field. */
let getThirdPartyNpmLibs libDir::libDir => {
  let packageJsonPath = Path.relative dir::libDir "package.json";
  let deps =
    [from_file (Path.to_string packageJsonPath)] |> Util.filter_member "dependencies" |> Util.filter_assoc;
  let libs =
    switch deps {
    | [x] => x |> List.map f::fst |> List.map f::(fun name => Utils.Lib name)
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
  | Some (`Assoc d) => d |> List.map f::fst |> List.map f::(fun name => Utils.Lib name)
  | _ => []
  }
};

/* Figure out the order in which third-party libs should be compiled, based on their dependencies (the
   depended is compiled before the dependent). */
let sortedTransitiveThirdPartyNpmLibsIncludingSelf's = {
  let thirdPartyLibs = getThirdPartyNpmLibs libDir::Path.the_root;
  let thirdPartyDeps =
    List.map
      thirdPartyLibs
      f::(
        fun name => {
          /* third party's own third party libs */
          let libDir = Path.relative dir::Utils.nodeModulesRoot (Utils.tsl name);
          (name, getThirdPartyNpmLibs libDir::libDir)
        }
      );
  /* `topologicalSort` will also return our own deps. */
  Utils.topologicalSort thirdPartyDeps
};

let transitiveThirdPartyOcamlfindLibsIncludingSelf's = {
  let thirdPartyNpmLibs = getThirdPartyNpmLibs libDir::Path.the_root;
  let thirdPartyOcamlfindLibs = getThirdPartyOcamlfindLibs libDir::Path.the_root;
  let thirdPartyLibDirs =
    List.map
      thirdPartyNpmLibs f::(fun name => Path.relative dir::Utils.nodeModulesRoot (Utils.tsl name));
  /* each third party's own third party libs */
  let thirdPartiesThirdPartyOcamlfindLibNamesD =
    List.map thirdPartyLibDirs f::(fun libDir => getThirdPartyOcamlfindLibs libDir::libDir);
  thirdPartyOcamlfindLibs @ List.concat thirdPartiesThirdPartyOcamlfindLibNamesD |> List.dedup
};
