open Yojson.Basic;

let packageJson = from_file "./package.json";

let asd = Util.filter_member "name" [packageJson] |> Util.filter_string |> List.hd;

let () = print_endline asd;

/* ocamlfind ocamlc -pp refmt -linkpkg -package yojson -g -impl extractName.re -o extractName 2>&1 | huh */
