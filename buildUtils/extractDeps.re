
open Yojson.Basic;

let packageJsonPath = Sys.argv.(1);

[from_file packageJsonPath] |>
  Util.filter_member "dependencies" |>
  Util.filter_assoc |>
  List.hd |>
  List.map fst |>
  String.concat "\n" |>
  print_endline;

/* ocamlfind ocamlc -pp refmt -linkpkg -package yojson -g -impl extractDeps.re -o extractDeps 2>&1 | huh */
