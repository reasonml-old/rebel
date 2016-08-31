open Yojson.Basic;

let packageJsonPath = Sys.argv.(1);

if (([from_file packageJsonPath] |> Util.filter_member "rebel" |> List.length) != 0) {
  print_string "true"
} else {
  print_string "false"
};
