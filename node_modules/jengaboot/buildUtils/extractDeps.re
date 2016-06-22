
open Yojson.Basic;

let packageJsonPath = Sys.argv.(1);

let deps = [from_file packageJsonPath] |> Util.filter_member "dependencies" |> Util.filter_assoc;

if (deps == []) {
  ()
} else {
  List.hd deps |> List.map fst |> String.concat "\n" |> print_endline
};
