open Core.Std;

open Jenga_lib.Api;

open Yojson.Basic;

/* general helpers */

let bash command => Action.process dir::Path.the_root prog::"bash" args::["-c", command] ();

let bashf fmt => ksprintf bash fmt;

let nonBlank s =>
  switch (String.strip s) {
  | "" => false
  | _ => true
  };

let relD dir::dir str => Dep.path (Path.relative dir::dir str);

/* assumes there is a suffix to chop. Throws otherwise */
let chopSuffixExn str => String.slice str 0 (String.rindex_exn str '.');

let fileNameNoExtNoDir path => Path.basename path |> chopSuffixExn;

let isInterface path => {
  let base = Path.basename path;
  String.is_suffix base suffix::".rei" || String.is_suffix base suffix::".mli"
};

/* this rebel-specific helpers */

type moduleName =
  | Mod string;

type libName =
  | Lib string;

let tsp = Path.to_string;

let tsm (Mod s) => s;

let tsl (Lib s) => s;

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

let libToModule (Lib name) => Mod (String.capitalize name |> kebabToCamel);

let pathToModule path => Mod (fileNameNoExtNoDir path |> String.capitalize);

let namespacedName libName::libName path::path =>
  tsm (libToModule libName) ^ "__" ^ tsm (pathToModule path);

  let nodeModulesRoot = Path.relative dir::Path.the_root "node_modules";

  let buildDirRoot = Path.relative dir::Path.the_root "_build";

  let topSrcDir = Path.relative dir::Path.the_root "src";

  let topLibName = {
    let packageJsonPath = Path.relative dir::Path.the_root "package.json";
    from_file (Path.to_string packageJsonPath) |> Util.member "name" |> Util.to_string |> (
      fun name => Lib name
    )
  };
