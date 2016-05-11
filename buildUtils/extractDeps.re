open Yojson.Basic;

let packageJson = from_file "./package.json";

let asd = Util.filter_member "dependencies" [packageJson] |> Util.filter_assoc |> List.hd;

let () = print_endline @@ String.concat "\n" @@ List.map fst asd;

/* ocamlfind ocamlc -pp refmt -linkpkg -package yojson -g -impl extractDeps.re -o extractDeps 2>&1 | huh */
