open Core.Std;

open Async.Std;

open Jenga_lib.Api;

let ( *>>| ) = Dep.map;

let ( *>>= ) = Dep.bind;

( *>>= );

( *>>| );

let () = ignore (List.iter [1] f::(fun _ => ()));

let rel = Path.relative;

let bash dir::dir command => Action.process dir::dir prog::"bash" args::["-c", command] ();

let bashf dir::dir fmt => ksprintf (fun str => bash dir::dir str) fmt;

let () = ignore (bashf dir::Path.the_root "asd");

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
    print_endline a;
    a
  } else {
    a
  };

tap;

let tapl n a =>
  if (n == 0) {
    List.iter f::print_endline a;
    a
  } else {
    a
  };

tapl;

let ocamldep dir::dir (sourcePaths: list Path.t) =>
  Dep.action_stdout (
    Dep.return (
      bashf
        dir::dir
        "ocamldep -pp refmt -modules -one-line %s"
        (
          sourcePaths |>
            List.map f::Path.to_string |> List.map f::(fun s => " -impl " ^ s) |> String.concat sep::" "
        )
    )
  ) *>>| (
  fun string =>
    List.map
      (split_into_lines string)
      f::(
        fun line => {
          let (target, deps) = parse_line line;
          (
            rel dir::dir target,
            List.map
              (
                tapl
                  1
                  (deps |> List.map f::(fun a => "_build/" ^ libName ^ "/" ^ String.uncapitalize a ^ ".cmi"))
              )
              f::(rel dir::dir)
          )
        }
      )
);

let ocamldepSort dir::dir (sourcePaths: list Path.t) =>
  Dep.action_stdout (
    Dep.return (
      bashf
        dir::dir
        "ocamldep -pp refmt -sort -one-line %s"
        (
          tap
            1
            (
              sourcePaths |>
                List.map f::Path.to_string |> List.map f::(fun s => " -impl " ^ s) |> String.concat sep::" "
            )
        )
    )
  ) *>>| (
  fun string => String.strip string |> tap 3 |> String.split on::' '
);

ocamldep;

ocamldepSort;

let findInOcamldep al item =>
  switch (List.Assoc.find al item) {
  | None => raise Not_found
  | Some xs =>
      /* ignore @@ tapl (Path.to_string item == "src/main.re" ? 0 : 1) (List.map xs f::Path.to_string);
         print_endline "---------"; */
      Dep.all_unit (List.map xs f::Dep.path)
  };

findInOcamldep;

let sortPathsTopologically dir::dir paths::paths =>
  Dep.action_stdout (
    Dep.return {
      let pathsString = List.map paths f::(fun a => " -impl " ^ Path.to_string a) |> String.concat sep::" ";
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
        (List.map sourcePaths f::(fun a => " -impl " ^ Path.to_string a) |> String.concat sep::"")
    )
  ) *>>| (
  fun string =>
    String.strip string |>
      String.split on::'\n' |>
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
  ignore dir;
  let srcDir = Path.root_relative "src";
  let buildDir = Path.root_relative ("_build/" ^ libName);
  ignore buildDir;
  ignore srcDir;
  Scheme.all [
    Scheme.rules_dep (
      Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
        fun rawPaths => sortPathsTopologically dir::Path.the_root paths::rawPaths *>>= (
          fun paths => getDepModules dir::Path.the_root sourcePaths::paths *>>| (
            fun assocList =>
              List.map
                paths
                f::(
                  fun pa => {
                    let name = Path.to_string pa;
                    let pathWithoutExt = String.chop_suffix_exn name suffix::".re";
                    let cmi = pathWithoutExt ^ ".cmi";
                    let cmo = pathWithoutExt ^ ".cmo";
                    let moduleDeps =
                      switch (List.Assoc.find assocList pa) {
                      | None => failwith ("lookup: " ^ name)
                      | Some modules =>
                          List.map
                            (modules |> tapl 1)
                            f::(fun m => Dep.path (rel dir::srcDir (String.uncapitalize m ^ ".re")))
                      };
                    Rule.create
                      targets::[
                        rel dir::Path.the_root cmi,
                        rel dir::Path.the_root cmo
                      ]
                      (
                        Dep.all_unit ([Dep.path pa] @ moduleDeps) *>>| (
                          fun () =>
                            bashf
                              dir::Path.the_root
                              "ocamlc -pp refmt -c -I src/ -o %s -impl %s"
                              pathWithoutExt
                              name
                        )
                      )
                  }
                )
          )
        )
      )
    ),
    Scheme.rules_dep (
      Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
        fun rawPaths => sortPathsTopologically dir::Path.the_root paths::rawPaths *>>| (
          fun paths => {
            let depsString =
              List.map
                paths f::(fun p => (Path.to_string p |> String.chop_suffix_exn suffix::".re") ^ ".cmo");
            let out = rel dir::srcDir "entry.out";
            [
              Rule.create
                targets::[out]
                (
                  Dep.all_unit (List.map depsString f::(fun d => Dep.path (rel dir::Path.the_root d))) *>>| (
                    fun () =>
                      bashf
                        dir::Path.the_root "ocamlc -o src/entry.out %s" (String.concat sep::" " depsString)
                  )
                )
            ]
          }
        )
      )
    ),
    Scheme.rules [Rule.default dir::Path.the_root [Dep.path (rel dir::srcDir "entry.out")]]
  ]
};

let env = Env.create scheme;

let setup () => Deferred.return env;
