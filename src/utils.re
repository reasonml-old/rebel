/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

open Jenga_lib.Api;

open Yojson.Basic;

/* String helpers */
let kebabToCamel =
  String.foldi
    init::""
    f::(
      fun _ accum char =>
        if (accum == "") {
          Char.to_string char
        } else if (accum.[String.length accum - 1] == '-') {
          String.slice accum 0 (-1) ^ (Char.to_string char |> String.capitalize)
        } else {
          accum ^ Char.to_string char
        }
    );

let nonBlank s =>
  switch (String.strip s) {
  | "" => false
  | _ => true
  };

/* assumes there is a suffix to chop. Throws otherwise */
let chopSuffixExn str => String.slice str 0 (String.rindex_exn str '.');

/* jenga helpers */
let bash command => Action.process dir::Path.the_root prog::"bash" args::["-c", command] ();

let bashf fmt => ksprintf bash fmt;

let relD dir::dir str => Dep.path (Path.relative dir::dir str);

let rel = Path.relative;

/* Flipped the operands so that it is easier to use with |> */
let bindD f dep => Dep.bind dep f;

let mapD f dep => Dep.map dep f;


/** Path helpers **/
let fileNameNoExtNoDir path => Path.basename path |> chopSuffixExn;

let isInterface path => {
  let base = Path.basename path;
  String.is_suffix base suffix::".rei" || String.is_suffix base suffix::".mli"
};

let hasInterface sourcePaths::sourcePaths path =>
  not (isInterface path) &&
  List.exists
    sourcePaths
    f::(fun path' => isInterface path' && fileNameNoExtNoDir path' == fileNameNoExtNoDir path);

let nodeModulesRoot = Path.relative dir::Path.the_root "node_modules";

let buildDirRoot = Path.relative dir::Path.the_root "_build";

let topSrcDir = Path.relative dir::Path.the_root "src";


/** this rebel-specific helpers **/
type moduleName =
  | Mod string;

type libName =
  | Lib string;

let tsp = Path.to_string;

let tsm (Mod s) => s;

let tsl (Lib s) => s;

let libToModule (Lib name) => Mod (String.capitalize name |> kebabToCamel);

let pathToModule path => Mod (fileNameNoExtNoDir path |> String.capitalize);

let namespacedName libName::libName path::path =>
  tsm (libToModule libName) ^ "__" ^ tsm (pathToModule path);

/* FIXME Remove after bloomberg/bucklescript#757 is fixed */
let bsNamespacedName libName::libName path::path => {
  let bsKebabToCamel =
    String.foldi
      init::""
      f::(
        fun _ accum char =>
          if (accum == "") {
            Char.to_string char |> String.lowercase
          } else if (
            accum.[String.length accum - 1] == '-'
          ) {
            String.slice accum 0 (-1) ^ (Char.to_string char |> String.capitalize)
          } else {
            accum ^ Char.to_string char
          }
      );
  let bsLibToModule (Lib name) => Mod (String.capitalize name |> bsKebabToCamel);
  tsm (bsLibToModule libName) ^ "__" ^ tsm (pathToModule path)
};

/* let topLibName = {
     let packageJsonPath = Path.relative dir::Path.the_root "package.json";
     from_file (Path.to_string packageJsonPath) |> Util.member "name" |> Util.to_string |> (
       fun name => Lib name
     )
   }; */
let topLibName = Lib "src";


/** FIXME What should be used as topLibName package name or src  */
/* let topLibName = "src"; */
/* Generic sorting algorithm on directed acyclic graph. Example: [(a, [b, c, d]), (b, [c]), (d, [c])] will be
   sorted into [c, d, b, a] or [c, b, d, a], aka the ones being depended on will always come before the
   dependent */
let topologicalSort graph => {
  let graph = {contents: graph};
  let rec topologicalSort' currNode accum => {
    let nodeDeps =
      switch (List.Assoc.find graph.contents currNode) {
      /* node not found: presume to be third-party dep. This is slightly dangerous because it might also mean
         we didn't construct the graph correctly. */
      | None => []
      | Some nodeDeps' => nodeDeps'
      };
    List.iter nodeDeps f::(fun dep => topologicalSort' dep accum);
    if (List.for_all accum.contents f::(fun n => n != currNode)) {
      accum := [currNode, ...accum.contents];
      graph := List.Assoc.remove graph.contents currNode
    }
  };
  let accum = {contents: []};
  while (not (List.is_empty graph.contents)) {
    topologicalSort' (fst (List.hd_exn graph.contents)) accum
  };
  List.rev accum.contents
};

/* package.json helpers */
let targets = {
  let packageJsonPath = Path.relative dir::Path.the_root "package.json";
  let targets' =
    from_file (Path.to_string packageJsonPath) |> Util.member "rebel" |>
    Util.to_option (fun a => a |> Util.member "targets");
  switch targets' {
  | Some (`List _ as a) => List.map f::Util.to_string (Util.to_list a)
  | None => []
  | _ => []
  }
};

let backend = {
  let packageJsonPath = Path.relative dir::Path.the_root "package.json";
  let optBackend =
    from_file (Path.to_string packageJsonPath) |> Util.member "rebel" |>
    Util.to_option (fun a => a |> Util.member "backend");
  switch optBackend {
  | Some (`String _ as a) =>
    let backend' = Util.to_string a;
    List.mem ["native", "jsoo", "bucklescript"] backend' ? backend' : "native"
  | _ => "native"
  }
};

let bscFlags = {
  let packageJsonPath = Path.relative dir::Path.the_root "package.json";
  let flags =
    from_file (Path.to_string packageJsonPath) |> Util.member "rebel" |>
    Util.to_option (fun a => a |> Util.member "bscFlags");
  switch flags {
  | Some (`String _ as a) => Util.to_string a
  | _ => ""
  }
};

let ocamlcFlags = {
  let packageJsonPath = Path.relative dir::Path.the_root "package.json";
  let flags =
    from_file (Path.to_string packageJsonPath) |> Util.member "rebel" |>
    Util.to_option (fun a => a |> Util.member "ocamlcFlags");
  switch flags {
  | Some (`String _ as a) => Util.to_string a
  | _ => ""
  }
};
