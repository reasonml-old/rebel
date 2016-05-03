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

/* let _ = Some 1 [@explicit_arity] */
let scheme dir::dir => {
  ignore dir;
  let srcDir = Path.root_relative "src";
  let buildDir = Path.root_relative ("_build/" ^ libName);
  ignore buildDir;
  ignore srcDir;
  Scheme.all [
    Scheme.rules_dep (
      Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
        fun unorderedSourcePaths => ocamldepSort dir::Path.the_root unorderedSourcePaths *>>= (
          fun orderedSourcePaths => ocamldep dir::Path.the_root unorderedSourcePaths *>>| (
            fun ocamldepAssocList => {
              let outRule =
                Rule.simple
                  targets::[rel dir::buildDir "done.out"]
                  deps::[
                    Dep.all_unit (
                      [Dep.path (rel dir::buildDir "main.cmo")] @ [
                        findInOcamldep ocamldepAssocList (rel dir::srcDir "main.re")
                      ]
                    )
                  ]
                  action::(
                    bashf
                      dir::Path.the_root
                      "ocamlc -o %s %s"
                      (Path.to_string (rel dir::buildDir "done.out"))
                      (
                        orderedSourcePaths |>
                          List.map
                            f::(
                              fun p =>
                                "_build/" ^
                                  libName ^
                                  "/" ^
                                  String.chop_suffix_exn (Path.basename (rel dir::buildDir p)) suffix::".re" ^
                                  ".cmo"
                            ) |>
                          String.concat sep::" "
                      )
                  );
              [outRule] @ (
                List.map orderedSourcePaths f::Path.root_relative |>
                  List.map
                    f::(
                      fun sourcePath => {
                        let name = String.chop_suffix_exn (Path.basename sourcePath) suffix::".re";
                        Rule.create
                          targets::[rel dir::buildDir (name ^ ".cmi"), rel dir::buildDir (name ^ ".cmo")]
                          {
                            let sourceRe = Path.to_string (rel dir::srcDir (name ^ ".re"));
                            let out = Path.to_string @@ rel dir::buildDir name;
                            Dep.all_unit (
                              [Dep.path sourcePath] @ [findInOcamldep ocamldepAssocList sourcePath]
                            )
                              *>>| (
                              fun () =>
                                bashf
                                  dir::Path.the_root
                                  "ocamlc -pp refmt -c -I %s -o %s -impl %s"
                                  (Path.to_string buildDir)
                                  out
                                  sourceRe
                            )
                          }
                      }
                    )
              )
            }
          )
        )
      )
    ),
    Scheme.rules [Rule.default dir::dir [Dep.path @@ rel dir::buildDir "done.out"]]
  ]
};

let env = Env.create scheme;

let setup () => Deferred.return env;
