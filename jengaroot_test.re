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

ts;

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
        (List.map sourcePaths f::(fun a => " -impl " ^ ts a) |> String.concat sep::"" |> tap 1)
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

let scheme dir::dir => {
  let srcDir = Path.root_relative "src";
  let buildDir = Path.root_relative ("_build/" ^ libName);
  let moduleAliasFilePath = rel dir::buildDir (libName ^ ".re");
  ignore dir;
  ignore buildDir;
  ignore srcDir;
  ignore moduleAliasFilePath;
  /* if (dir != Path.the_root) {
       Scheme.no_rules
     } else { */
  Scheme.all [
    /* module alias generation */
    Scheme.rules_dep (
      Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>| (
        fun paths => {
          /* Printing all the globbed results. outout.re is being captured here. */
          /* List.map paths f::Path.to_string |> List.iter f::print_endline;
             print_endline @@ (Path.to_string dir ^ "================"); */
          /* HACK: As a temporarily workaround, I can filter out _build/outout.re. If you uncomment next line
             there'd be no more cycles */
          let paths = List.filter paths f::(fun p => not (Path.is_descendant dir::buildDir p));
          let moduleAliasFileContent =
            List.map
              (taplp 1 paths)
              f::(
                fun p => {
                  let name = Path.basename p |> String.chop_suffix_exn suffix::".re";
                  Printf.sprintf
                    "let module %s = %s__%s;" (String.capitalize name) (String.capitalize libName) name
                }
              ) |>
              String.concat sep::"\n";
          /* List.map paths f::Path.to_string |> List.iter f::print_endline;
             print_endline @@ (ts dir ^ "================"); */
          [
            Rule.create
              targets::[moduleAliasFilePath]
              (
                Dep.all_unit (List.map paths f::Dep.path) *>>| (
                  fun () =>
                    bashf
                      dir::Path.the_root
                      "echo %s > %s"
                      (Shell.escape moduleAliasFileContent)
                      (ts moduleAliasFilePath)
                )
              )
          ]
        }
      )
    ),
    /* compiling, no linking */
    Scheme.rules_dep (
      Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
        fun rawPaths => sortPathsTopologically dir::Path.the_root paths::rawPaths *>>= (
          fun paths => getDepModules dir::Path.the_root sourcePaths::paths *>>| (
            fun assocList => {
              /* same hack, check above */
              let paths = List.filter paths f::(fun p => not (Path.is_descendant dir::buildDir p));
              List.map
                paths
                f::(
                  fun pa => {
                    let name = ts pa;
                    let outPathWithoutExt =
                      String.chop_suffix_exn (Path.basename pa) suffix::".re" |> rel dir::buildDir |> ts;
                    let cmi = outPathWithoutExt ^ ".cmi";
                    let cmo = outPathWithoutExt ^ ".cmo";
                    let moduleDeps =
                      switch (List.Assoc.find assocList pa) {
                      | None => failwith ("lookup: " ^ name)
                      | Some modules =>
                          List.map
                            (modules |> tapl 1)
                            /* compiling here only needs cmi */
                            f::(fun m => Dep.path (rel dir::buildDir (String.uncapitalize m ^ ".cmi")))
                      };
                    Rule.create
                      targets::[rel dir::Path.the_root cmi, rel dir::Path.the_root cmo]
                      (
                        Dep.all_unit [Dep.path pa, ...moduleDeps] *>>| (
                          fun () =>
                            bashf
                              dir::Path.the_root
                              "ocamlc -pp refmt -c -I %s -o %s -impl %s"
                              (ts buildDir)
                              outPathWithoutExt
                              name
                        )
                      )
                  }
                )
            }
          )
        )
      )
    ),
    /* linking */
    Scheme.rules_dep (
      Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
        fun rawPaths => sortPathsTopologically dir::Path.the_root paths::rawPaths *>>| (
          fun paths => {
            let paths = List.filter paths f::(fun p => not (Path.is_descendant dir::buildDir p));
            let depsString =
              List.map
                (taplp 1 paths)
                f::(
                  fun p => (
                    Path.basename p |> String.chop_suffix_exn suffix::".re" |> rel dir::buildDir |> ts
                  ) ^ ".cmo"
                );
            let out = rel dir::buildDir "entry.out";
            [
              Rule.create
                targets::[out]
                (
                  Dep.all_unit (List.map depsString f::(fun d => Dep.path (rel dir::Path.the_root d))) *>>| (
                    fun () =>
                      bashf dir::Path.the_root "ocamlc -o %s %s" (ts out) (String.concat sep::" " depsString)
                  )
                )
            ]
          }
        )
      )
    ),
    Scheme.rules [
      Rule.default
        dir::Path.the_root [Dep.path (rel dir::buildDir "entry.out"), Dep.path moduleAliasFilePath]
    ]
  ]
  /* } */
};

let env = Env.create scheme;

let setup () => Deferred.return env;
