open Core.Std
open Async.Std
open Jenga_lib.Api
let ( *>>| ) = Dep.map
let ( *>>= ) = Dep.bind
let _ = ( *>>= )
let _ = ( *>>| )
let () = ignore (List.iter [1] ~f:(fun _  -> ()))
let rel = Path.relative
let ts = Path.to_string
let _ = ts
let bash ~dir  command =
  Action.process ~dir ~prog:"bash" ~args:["-c"; command] ()
let bashf ~dir  fmt = ksprintf (fun str  -> bash ~dir str) fmt
let () = ignore (bashf ~dir:Path.the_root "asd")
let non_blank s = match String.strip s with | "" -> false | _ -> true
let split_into_lines string =
  List.filter ~f:non_blank (String.split ~on:'\n' string)
let split_into_words string =
  List.filter ~f:non_blank (String.split ~on:' ' string)
let libName = "hi"
let parse_line line =
  let err s = failwith (line ^ (" -- " ^ s)) in
  match String.split line ~on:':' with
  | before::after::[] ->
      (((match split_into_words before with
         | target::[] -> target
         | _ ->
             err
               "expected exactly one word before ':' in ocamldep output line")),
        (split_into_words after))
  | _ -> err "expected exactly one ':' in ocamldep output line"
let _ = non_blank
let _ = split_into_lines
let _ = split_into_words
let _ = parse_line
let tap n a = if n = 0 then (print_endline a; a) else a
let _ = tap
let tapl n a = if n = 0 then (List.iter ~f:print_endline a; a) else a
let _ = tapl
let ocamldep ~dir  (sourcePaths : Path.t list) =
  (Dep.action_stdout
     (Dep.return
        (bashf ~dir "ocamldep -pp refmt -modules -one-line %s"
           (((sourcePaths |> (List.map ~f:ts)) |>
               (List.map ~f:(fun s  -> " -impl " ^ s)))
              |> (String.concat ~sep:" ")))))
    *>>|
    (fun string  ->
       List.map (split_into_lines string)
         ~f:(fun line  ->
               let (target,deps) = parse_line line in
               ((rel ~dir target),
                 (List.map
                    (tapl 1
                       (deps |>
                          (List.map
                             ~f:(fun a  ->
                                   "_build/" ^
                                     (libName ^
                                        ("/" ^
                                           ((String.uncapitalize a) ^ ".cmi")))))))
                    ~f:(rel ~dir)))))
let ocamldepSort ~dir  (sourcePaths : Path.t list) =
  (Dep.action_stdout
     (Dep.return
        (bashf ~dir "ocamldep -pp refmt -sort -one-line %s"
           (tap 1
              (((sourcePaths |> (List.map ~f:ts)) |>
                  (List.map ~f:(fun s  -> " -impl " ^ s)))
                 |> (String.concat ~sep:" "))))))
    *>>|
    (fun string  ->
       ((String.strip string) |> (tap 3)) |> (String.split ~on:' '))
let _ = ocamldep
let _ = ocamldepSort
let findInOcamldep al item =
  match List.Assoc.find al item with
  | None  -> raise Not_found
  | ((Some (xs))) -> Dep.all_unit (List.map xs ~f:Dep.path)
let _ = findInOcamldep
let sortPathsTopologically ~dir  ~paths  =
  (Dep.action_stdout
     (Dep.return
        (let pathsString =
           (List.map paths ~f:(fun a  -> " -impl " ^ (ts a))) |>
             (String.concat ~sep:" ") in
         bashf ~dir "ocamldep -pp refmt -sort -one-line %s" pathsString)))
    *>>|
    (fun string  ->
       ((String.split string ~on:' ') |> (List.filter ~f:non_blank)) |>
         (List.map ~f:(rel ~dir)))
let _ = sortPathsTopologically
let getDepModules ~dir  ~sourcePaths  =
  (Dep.action_stdout
     (Dep.return
        (bashf ~dir "ocamldep -pp refmt -modules -one-line %s"
           ((List.map sourcePaths ~f:(fun a  -> " -impl " ^ (ts a))) |>
              (String.concat ~sep:"")))))
    *>>|
    (fun string  ->
       ((String.strip string) |> (String.split ~on:'\n')) |>
         (List.map
            ~f:(fun line  ->
                  match (String.strip line) |> (String.split ~on:':') with
                  | path::deps::[] ->
                      ((rel ~dir path),
                        ((String.split deps ~on:' ') |>
                           (List.filter ~f:non_blank)))
                  | _ ->
                      failwith
                        "expected exactly one ':' in ocamldep output line")))
let _ = getDepModules
let scheme ~dir  =
  ignore dir;
  (let srcDir = Path.root_relative "src" in
   let buildDir = Path.root_relative ("_build/" ^ libName) in
   ignore buildDir;
   ignore srcDir;
   Scheme.all
     [Scheme.rules_dep
        ((Dep.glob_listing (Glob.create ~dir:srcDir "*.re")) *>>=
           (fun rawPaths  ->
              (sortPathsTopologically ~dir:Path.the_root ~paths:rawPaths)
                *>>=
                (fun paths  ->
                   (getDepModules ~dir:Path.the_root ~sourcePaths:paths) *>>|
                     (fun assocList  ->
                        List.map paths
                          ~f:(fun pa  ->
                                let name = ts pa in
                                let outPathWithoutExt =
                                  ((String.chop_suffix_exn (Path.basename pa)
                                      ~suffix:".re")
                                     |> (rel ~dir:buildDir))
                                    |> ts in
                                let cmi = outPathWithoutExt ^ ".cmi" in
                                let cmo = outPathWithoutExt ^ ".cmo" in
                                let moduleDeps =
                                  match List.Assoc.find assocList pa with
                                  | None  -> failwith ("lookup: " ^ name)
                                  | ((Some (modules))) ->
                                      List.map (modules |> (tapl 1))
                                        ~f:(fun m  ->
                                              Dep.path
                                                (rel ~dir:buildDir
                                                   ((String.uncapitalize m) ^
                                                      ".cmi"))) in
                                Rule.create
                                  ~targets:[rel ~dir:Path.the_root cmi;
                                           rel ~dir:Path.the_root cmo]
                                  ((Dep.all_unit ([Dep.path pa] @ moduleDeps))
                                     *>>|
                                     (fun ()  ->
                                        bashf ~dir:Path.the_root
                                          "ocamlc -pp refmt -c -I %s -o %s -impl %s"
                                          (ts buildDir) outPathWithoutExt
                                          name)))))));
     Scheme.rules_dep
       ((Dep.glob_listing (Glob.create ~dir:srcDir "*.re")) *>>=
          (fun rawPaths  ->
             (sortPathsTopologically ~dir:Path.the_root ~paths:rawPaths) *>>|
               (fun paths  ->
                  let depsString =
                    List.map paths
                      ~f:(fun p  ->
                            ((((Path.basename p) |>
                                 (String.chop_suffix_exn ~suffix:".re"))
                                |> (rel ~dir:buildDir))
                               |> ts)
                              ^ ".cmo") in
                  let out = rel ~dir:buildDir "entry.out" in
                  [Rule.create ~targets:[out]
                     ((Dep.all_unit
                         (List.map depsString
                            ~f:(fun d  -> Dep.path (rel ~dir:Path.the_root d))))
                        *>>|
                        (fun ()  ->
                           bashf ~dir:Path.the_root "ocamlc -o %s %s"
                             (ts out) (String.concat ~sep:" " depsString)))])));
     Scheme.rules
       [Rule.default ~dir:Path.the_root
          [Dep.path (rel ~dir:buildDir "entry.out")]]])
let env = Env.create scheme
let setup () = Deferred.return env
