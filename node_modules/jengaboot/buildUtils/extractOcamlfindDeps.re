/* extracts `jengaboot.ocamlfindDependencies`, an object like the regular `dependencies` field */

open Yojson.Basic;

let packageJsonPath = Sys.argv.(1);

let deps =
  from_file packageJsonPath |>
    Util.member "jengaboot" |> Util.to_option (fun a => a |> Util.member "ocamlfindDependencies");

switch deps {
| Some (`Assoc d) => d |> List.map fst |> String.concat "\n" |> print_endline
| _ => ()
};
