open Core.Std;

open Async.Std;

open Jenga_lib.Api;

let ( *>>| ) = Dep.map;

let ( *>>= ) = Dep.bind;

( *>>= );

( *>>| );

let () = ignore (List.iter [1] f::(fun _ => ()));

let rel = Path.relative;

let ts = Path.to_string;

let root = Path.the_root;

ts;

rel;

root;

let bash dir::dir command => Action.process dir::dir prog::"bash" args::["-c", command] ();

let bashf dir::dir fmt => ksprintf (fun str => bash dir::dir str) fmt;

let () = ignore (bashf dir::Path.the_root "asdaaaa");

let non_blank s =>
  switch (String.strip s) {
  | "" => false
  | _ => true
  };

let split_into_lines string => List.filter f::non_blank (String.split on::'\n' string);

let split_into_words string => List.filter f::non_blank (String.split on::' ' string);

let libName = "hi";

libName;

let parse_line line => {
  let err s => failwith (line ^ " -- " ^ s);
  switch (String.split line on::':') {
  | [before, after] => (
      switch (split_into_words before) {
      | [target] => target
      | _ => err "expected exactly one word before ':' in ocamldep output line"
      },
      split_into_words after
    )
  | _ => err "expected exactly one ':' in ocamldep output line"
  }
};

non_blank;

split_into_lines;

split_into_words;

parse_line;

let tap n a =>
  if (n == 0) {
    print_endline "tap---------";
    print_endline a;
    a
  } else {
    a
  };

tap;

let tapl n a =>
  if (n == 0) {
    print_endline "tapl---------";
    List.iter f::print_endline a;
    a
  } else {
    a
  };

tapl;

let taplp n a =>
  if (n == 0) {
    print_endline "taplp---------";
    List.iter f::(fun a => print_endline (ts a)) a;
    a
  } else {
    a
  };

taplp;

let tapp n a =>
  if (n == 0) {
    print_endline "tapp---------";
    print_endline @@ ts a;
    a
  } else {
    a
  };

tapp;

let sortPathsTopologically dir::dir paths::paths =>
  /* List.map paths f::ts |> List.iter f::print_endline; */
  Dep.action_stdout (
    Dep.return {
      let pathsString = List.map paths f::(fun a => " -impl " ^ ts a) |> String.concat sep::" ";
      bashf dir::dir "ocamldep -pp refmt -sort -one-line %s" pathsString
    }
  ) *>>| (
  fun string => String.split string on::' ' |> List.filter f::non_blank |> List.map f::(rel dir::dir)
);

sortPathsTopologically;

let getDepModules dir::dir sourcePaths::sourcePaths =>
  Dep.action_stdout (
    Dep.return (
      bashf
        dir::dir
        "ocamldep -pp refmt -modules -one-line %s"
        (List.map (taplp 1 sourcePaths) f::(fun a => " -impl " ^ ts a) |> String.concat sep::"" |> tap 1)
    )
  ) *>>| (
  fun string =>
    String.strip string |>
      String.split on::'\n' |>
      List.filter f::non_blank |>
      List.map
        f::(
          fun line =>
            switch (String.strip line |> String.split on::':') {
            | [path, deps] => (rel dir::dir path, String.split deps on::' ' |> List.filter f::non_blank)
            | _ => failwith "expected exactly one ':' in ocamldep output line"
            }
        )
);

getDepModules;

let fileNameNoExtNoDir path suffix::suffix => Path.basename path |> String.chop_suffix_exn suffix::suffix;

fileNameNoExtNoDir;

let srcDir = Path.root_relative "src";

srcDir;

let filterOutModuleAliasFile paths =>
  List.filter paths f::(fun path => fileNameNoExtNoDir path suffix::".re" != libName);

let scheme dir::dir => {
  ignore dir;
  print_endline @@ (ts dir ^ "<<<<<<<<<<<<<<<");
  if (dir == root) {
    Scheme.rules_dep (
      Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>| (
        fun paths =>
          List.map
            (filterOutModuleAliasFile paths)
            f::(
              fun path => {
                ignore path;
                let outNameNoExtNoDir = libName ^ "__" ^ fileNameNoExtNoDir path suffix::".re";
                let outCmi = rel dir::srcDir (outNameNoExtNoDir ^ ".cmi") |> tapp 1;
                let outCmo = rel dir::srcDir (outNameNoExtNoDir ^ ".cmo");
                Rule.default dir::dir [Dep.path (tapp 1 outCmi), Dep.path outCmo]
              }
            )
      )
    )
  } else if (
    dir == srcDir
  ) {
    let moduleAliasFilePath = rel dir::srcDir (libName ^ ".re");
    let moduleAliasCmoPath = rel dir::srcDir (libName ^ ".cmo");
    let moduleAliasCmiPath = rel dir::srcDir (libName ^ ".cmi");
    Scheme.all [
      Scheme.rules_dep (
        Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
          fun unsortedPaths => {
            let filteredUnsortedPaths = filterOutModuleAliasFile unsortedPaths;
            sortPathsTopologically dir::root paths::filteredUnsortedPaths *>>= (
              fun sortedPaths => getDepModules dir::root sourcePaths::sortedPaths *>>| (
                fun assocList => {
                  /* module alias file generation */
                  let moduleAliasContent =
                    List.map
                      filteredUnsortedPaths
                      f::(
                        fun path => {
                          let name = fileNameNoExtNoDir path suffix::".re";
                          Printf.sprintf
                            "let module %s = %s__%s;"
                            (String.capitalize name)
                            (String.capitalize libName)
                            name
                        }
                      ) |>
                      String.concat sep::"\n";
                  let moduleAliasContentRules = [
                    Rule.create
                      targets::[moduleAliasFilePath]
                      (
                        Dep.all_unit (List.map filteredUnsortedPaths f::Dep.path) *>>| (
                          fun () =>
                            bashf
                              dir::root
                              "echo %s > %s"
                              (Shell.escape moduleAliasContent)
                              (ts (tapp 1 moduleAliasFilePath))
                        )
                      ),
                    Rule.default dir::dir [Dep.path moduleAliasFilePath]
                  ];
                  let moduleAliasCompileRules = [
                    Rule.create
                      targets::[moduleAliasCmoPath, tapp 1 moduleAliasCmiPath]
                      (
                        Dep.path moduleAliasFilePath *>>| (
                          fun () =>
                            bashf
                              dir::Path.the_root
                              "ocamlc -pp refmt -g -no-alias-deps -w -49 -c -impl %s -o %s"
                              (ts moduleAliasFilePath)
                              (ts moduleAliasCmoPath)
                        )
                      ),
                    Rule.default dir::dir [Dep.path moduleAliasCmoPath, Dep.path moduleAliasCmiPath]
                  ];
                  /* List.iter
                    assocList
                    f::(
                      fun (path, lst) => {
                        print_endline "==========";
                        print_string @@ (ts path ^ ", ");
                        List.iter lst f::print_endline;
                        print_endline "=========="
                      }
                    ); */
                  let sourcesCompileRules =
                    List.concat_map
                      sortedPaths
                      f::(
                        fun path => {
                          let modules =
                            switch (List.Assoc.find assocList path) {
                            | None => failwith ("lookup: " ^ ts path)
                            | Some modules => modules
                            };
                          let moduleDeps =
                            List.map
                              (tapl 1 modules)
                              /* compiling here only needs cmi */
                              f::(
                                fun m => Dep.path (
                                  rel dir::srcDir (libName ^ "__" ^ String.uncapitalize m ^ ".cmi")
                                )
                              );
                          let outNameNoExtNoDir = libName ^ "__" ^ fileNameNoExtNoDir path suffix::".re";
                          let outCmi = rel dir::srcDir (outNameNoExtNoDir ^ ".cmi") |> tapp 1;
                          let outCmo = rel dir::srcDir (outNameNoExtNoDir ^ ".cmo");
                          [
                            Rule.create
                              targets::[outCmi, outCmo]
                              (
                                Dep.all_unit [Dep.path path, Dep.path moduleAliasCmiPath, ...moduleDeps] *>>| (
                                  fun () =>
                                    bashf
                                      dir::srcDir
                                      /* |> ocamlc -pp refmt -g -open Chenglou -I _build/chenglou  -o _build/chenglou/chenglou__test.cmo -intf-suffix rei -c -impl ./node_modules/chenglou/test.re \ */
                                      "ocamlc -pp refmt -g -open %s -I %s -o %s -intf-suffix .rei -c -impl %s"
                                      (String.capitalize libName)
                                      (ts srcDir)
                                      outNameNoExtNoDir
                                      (Path.basename path)
                                )
                              ),
                            Rule.default dir::dir [Dep.path outCmi, Dep.path outCmo]
                          ]
                        }
                      );
                  moduleAliasContentRules @ moduleAliasCompileRules @ sourcesCompileRules
                }
              )
            )
          }
        )
      )
    ]
  } else {
    Scheme.no_rules
  }
};

let env = Env.create scheme;

let setup () => Deferred.return env;
