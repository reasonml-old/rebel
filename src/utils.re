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


/** OS helpers */
let os_name = {
  let ic = Unix.open_process_in "uname";
  let uname = input_line ic;
  In_channel.close ic;
  uname
};


/** jenga helpers */
let bash ignore_stderr::ignore_stderr command => Action.process dir::Path.the_root prog::"bash" args::["-c", command] ignore_stderr::ignore_stderr ();

let bashf ignore_stderr::ignore_stderr=false fmt => ksprintf (bash ignore_stderr::ignore_stderr) fmt;

let relD dir::dir str => Dep.path (Path.relative dir::dir str);

let rel = Path.relative;

let tsp = Path.to_string;

/* Flipped the operands so that it is easier to use with |> */
let bindD f dep => Dep.bind dep f;

let mapD f dep => Dep.map dep f;


/** Constants Paths */
let nodeModulesRoot = rel dir::Path.the_root "node_modules";

let build = rel dir::Path.the_root "_build";

let topSrcDir = rel dir::Path.the_root "src";


/** Path helpers **/
let fileNameNoExtNoDir path => Path.basename path |> chopSuffixExn;

let isInterface path => {
  let base = Path.basename path;
  String.is_suffix base suffix::".rei" || String.is_suffix base suffix::".mli"
};

let isImplementation path => not (isInterface path);

let hasInterface sourcePaths::sourcePaths path =>
  not (isInterface path) &&
  List.exists
    sourcePaths
    f::(fun path' => isInterface path' && fileNameNoExtNoDir path' == fileNameNoExtNoDir path);

let getSubDirs dir::dir =>
  Core.Core_sys.ls_dir (tsp dir) |>
  List.filter f::(fun subDir => Core.Core_sys.is_directory_exn (tsp (rel dir::dir subDir))) |>
  List.map f::(fun subDir => rel dir::dir subDir);

let rec getNestedSubDirs dir::dir =>
  List.fold
    (getSubDirs dir)
    init::[]
    f::(
      fun acc subDir => {
        let nestedSubDirs = getNestedSubDirs dir::subDir;
        acc @ [subDir] @ nestedSubDirs
      }
    );

let isImplOrIntfFile file =>
  String.is_suffix file suffix::".rei" ||
  String.is_suffix file suffix::".mli" ||
  String.is_suffix file suffix::".re" || String.is_suffix file suffix::".ml";

let getSourceFiles dir::dir => {
  let allDirs = [dir] @ getNestedSubDirs dir::dir;
  let getFilesInDir acc subDir =>
    (
      Core.Core_sys.ls_dir (tsp subDir) |> List.filter f::isImplOrIntfFile |>
      List.map f::(fun file => rel dir::subDir file)
    ) @ acc;
  List.fold allDirs init::[] f::getFilesInDir
};


/** Build Path Helpers **/
let extractTargetName dir::dir => {
  let pathComponents = String.split on::'/' (tsp dir);
  List.nth_exn pathComponents 1
};

let extractPackageName dir::dir => {
  let pathComponents = String.split on::'/' (tsp dir);
  List.nth_exn pathComponents 2
};

let convertBuildDirToLibDir buildDir::buildDir target::target => {
  let path = String.chop_prefix_exn (tsp buildDir) (tsp (rel dir::build target) ^ "/");
  let pathComponents = String.split path on::'/';

  /** prepare base src path */
  let packageName = extractPackageName dir::buildDir;
  let libRoot = packageName == "src" ? Path.the_root : rel dir::nodeModulesRoot packageName;
  let basePath = rel dir::libRoot "src";

  /** TODO write examples */
  if (List.length pathComponents == 1) {
    basePath
  } else {
    List.slice pathComponents 1 (List.length pathComponents) |> String.concat sep::"/" |>
    rel dir::basePath
  }
};


/** Rebel-specific helpers **/
type moduleName =
  | Mod string;

type libName =
  | Lib string;

let tsm (Mod s) => s;

let tsl (Lib s) => s;

let libToModule (Lib name) => Mod (String.capitalize name |> kebabToCamel);

let pathToModule path => Mod (fileNameNoExtNoDir path |> String.capitalize);

let namespacedName libName::libName path::path =>
  tsm (libToModule libName) ^ "__" ^ tsm (pathToModule path);

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

/* FIXME Remove after bloomberg/bucklescript#757 is fixed */
let bsNamespacedName libName::libName path::path =>
  /** On OSX files are not case-insensitive so this unnecessary  */
  os_name == "Darwin" ?
    namespacedName libName::libName path::path :
    tsm (bsLibToModule libName) ^ "__" ^ tsm (pathToModule path);

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
type flags = {dep: string, compile: string, link: string, jsoo: string, envvars: string};

type target = {
  target: string,
  engine: string,
  entry: string,
  compiler: string,
  cmox: string,
  cmax: string,
  flags: flags
};

type config = {targets: list target};

let parseTarget t => {
  let target = Util.member "target" t |> Util.to_string;
  let engine = Util.member "engine" t |> Util.to_string;
  let entry = Util.member "entry" t |> Util.to_string;
  let flags = t |> Util.to_option (fun a => a |> Util.member "unstable_flags");
  let flags =
    switch flags {
    | Some f => f
    | None => `Assoc []
    };
  let dep =
    switch (flags |> Util.to_option (fun a => a |> Util.member "dep")) {
    | Some (`String _ as a) => Util.to_string a
    | _ => ""
    };
  let compile =
    switch (flags |> Util.to_option (fun a => a |> Util.member "compile")) {
    | Some (`String _ as a) => Util.to_string a
    | _ => ""
    };
  let link =
    switch (flags |> Util.to_option (fun a => a |> Util.member "link")) {
    | Some (`String _ as a) => Util.to_string a
    | _ => ""
    };
  let jsoo =
    switch (flags |> Util.to_option (fun a => a |> Util.member "jsoo")) {
    | Some (`String f) => f
    | _ => ""
    };
  let envvars =
    switch (flags |> Util.to_option (fun a => a |> Util.member "envvars")) {
    | Some (`String f) => f
    | _ => ""
    };
  let (compiler, cmox, cmax) =
    switch engine {
    | "native" => ("ocamlopt", ".cmx", ".cmxa")
    /* Because jsoo needs byte code generated by ocamlc artifacts. */
    | "jsoo"
    | "byte" => ("ocamlc", ".cmo", ".cma")
    | _ => ("", "", "")
    };
  {target, engine, entry, compiler, cmox, cmax, flags: {dep, compile, link, jsoo, envvars}}
};

let defaultTarget = {
  target: "default",
  engine: "native",
  entry: "src/Index.re",
  compiler: "ocamlopt",
  cmox: ".cmx",
  cmax: ".cmxa",
  flags: {dep: "", compile: "", link: "", jsoo: "", envvars: ""}
};

let rebelConfig = {
  let packageJsonPath = Path.relative dir::Path.the_root "package.json";
  let configFile = from_file (Path.to_string packageJsonPath);
  let targetsField =
    configFile |> Util.member "rebel" |> Util.to_option (fun a => a |> Util.member "targets");
  let targets =
    switch targetsField {
    | Some (`List ts) => List.map f::parseTarget ts
    | _ => [defaultTarget]
    };
  {targets: targets}
};

let findTarget name => {
  let pos = List.foldi rebelConfig.targets init::0 f::(fun i acc t => t.target == name ? i : acc);
  List.nth_exn rebelConfig.targets pos
};

let readFile path::path =>
  switch (Core.Core_sys.is_file (tsp path)) {
  | `Yes => In_channel.read_all (tsp path)
  | _ => ""
  };


/** Debugging Utils */
let print_path path => print_endline @@ tsp path;

let print_paths paths => List.iter paths f::print_path;
