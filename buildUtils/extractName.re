
open Yojson.Basic;

let packageJsonPath = Sys.argv.(1);

[from_file packageJsonPath] |>
  Util.filter_member "name" |> Util.filter_string |> List.hd |> print_endline;

/* ocamlfind ocamlc -pp refmt -linkpkg -package yojson -g -impl extractName.re -o extractName 2>&1 | huh */
