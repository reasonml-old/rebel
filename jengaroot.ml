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
let root = Path.the_root
let _ = ts
let _ = rel
let _ = root
let bash ~dir  command =
  Action.process ~dir ~prog:"bash" ~args:["-c"; command] ()
let bashf ~dir  fmt = ksprintf (fun str  -> bash ~dir str) fmt
let () = ignore (bashf ~dir:Path.the_root "asdaaaa")
let non_blank s = match String.strip s with | "" -> false | _ -> true
let split_into_lines string =
  List.filter ~f:non_blank (String.split ~on:'\n' string)
let split_into_words string =
  List.filter ~f:non_blank (String.split ~on:' ' string)
let libName = "hi"
let _ = libName
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
let tap n a =
  if n = 0 then (print_endline "tap---------"; print_endline a; a) else a
let _ = tap
let tapl n a =
  if n = 0
  then (print_endline "tapl---------"; List.iter ~f:print_endline a; a)
  else a
let _ = tapl
let taplp n a =
  if n = 0
  then
    (print_endline "taplp---------";
     List.iter ~f:(fun a  -> print_endline (ts a)) a;
     a)
  else a
let _ = taplp
let tapp n a =
  if n = 0
  then (print_endline "tapp---------"; print_endline @@ (ts a); a)
  else a
let _ = tapp
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
           (((List.map (taplp 1 sourcePaths)
                ~f:(fun a  -> " -impl " ^ (ts a)))
               |> (String.concat ~sep:""))
              |> (tap 1)))))
    *>>|
    (fun string  ->
       (((String.strip string) |> (String.split ~on:'\n')) |>
          (List.filter ~f:non_blank))
         |>
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
let fileNameNoExtNoDir path ~suffix  =
  (Path.basename path) |> (String.chop_suffix_exn ~suffix)
let _ = fileNameNoExtNoDir
let srcDir = Path.root_relative "src"
let _ = srcDir
let filterOutModuleAliasFile paths =
  List.filter paths
    ~f:(fun path  -> (fileNameNoExtNoDir path ~suffix:".re") <> libName)
let scheme ~dir  =
  ignore dir;
  print_endline @@ ((ts dir) ^ "<<<<<<<<<<<<<<<");
  if dir = root
  then
    Scheme.rules_dep
      ((Dep.glob_listing (Glob.create ~dir:srcDir "*.re")) *>>|
         (fun paths  ->
            List.map (filterOutModuleAliasFile paths)
              ~f:(fun path  ->
                    ignore path;
                    (let outNameNoExtNoDir =
                       libName ^
                         ("__" ^ (fileNameNoExtNoDir path ~suffix:".re")) in
                     let outCmi =
                       (rel ~dir:srcDir (outNameNoExtNoDir ^ ".cmi")) |>
                         (tapp 1) in
                     let outCmo =
                       rel ~dir:srcDir (outNameNoExtNoDir ^ ".cmo") in
                     Rule.default ~dir
                       [Dep.path (tapp 1 outCmi); Dep.path outCmo]))))
  else
    if dir = srcDir
    then
      (let moduleAliasFilePath = rel ~dir:srcDir (libName ^ ".re") in
       let moduleAliasCmoPath = rel ~dir:srcDir (libName ^ ".cmo") in
       let moduleAliasCmiPath = rel ~dir:srcDir (libName ^ ".cmi") in
       Scheme.all
         [Scheme.rules_dep
            ((Dep.glob_listing (Glob.create ~dir:srcDir "*.re")) *>>=
               (fun unsortedPaths  ->
                  let filteredUnsortedPaths =
                    filterOutModuleAliasFile unsortedPaths in
                  (sortPathsTopologically ~dir:root
                     ~paths:filteredUnsortedPaths)
                    *>>=
                    (fun sortedPaths  ->
                       (getDepModules ~dir:root ~sourcePaths:sortedPaths)
                         *>>|
                         (fun assocList  ->
                            let moduleAliasContent =
                              (List.map filteredUnsortedPaths
                                 ~f:(fun path  ->
                                       let name =
                                         fileNameNoExtNoDir path
                                           ~suffix:".re" in
                                       Printf.sprintf
                                         "let module %s = %s__%s;"
                                         (String.capitalize name)
                                         (String.capitalize libName) name))
                                |> (String.concat ~sep:"\n") in
                            let moduleAliasContentRules =
                              [Rule.create ~targets:[moduleAliasFilePath]
                                 ((Dep.all_unit
                                     (List.map filteredUnsortedPaths
                                        ~f:Dep.path))
                                    *>>|
                                    (fun ()  ->
                                       bashf ~dir:root "echo %s > %s"
                                         (Shell.escape moduleAliasContent)
                                         (ts (tapp 1 moduleAliasFilePath))));
                              Rule.default ~dir
                                [Dep.path moduleAliasFilePath]] in
                            let moduleAliasCompileRules =
                              [Rule.create
                                 ~targets:[moduleAliasCmoPath;
                                          tapp 1 moduleAliasCmiPath]
                                 ((Dep.path moduleAliasFilePath) *>>|
                                    (fun ()  ->
                                       bashf ~dir:Path.the_root
                                         "ocamlc -pp refmt -g -no-alias-deps -w -49 -c -impl %s -o %s"
                                         (ts moduleAliasFilePath)
                                         (ts moduleAliasCmoPath)));
                              Rule.default ~dir
                                [Dep.path moduleAliasCmoPath;
                                Dep.path moduleAliasCmiPath]] in
                            let sourcesCompileRules =
                              List.concat_map sortedPaths
                                ~f:(fun path  ->
                                      let modules =
                                        match List.Assoc.find assocList path
                                        with
                                        | None  ->
                                            failwith ("lookup: " ^ (ts path))
                                        | ((Some
                                            (modules))) ->
                                            modules in
                                      let moduleDeps =
                                        List.map (tapl 1 modules)
                                          ~f:(fun m  ->
                                                Dep.path
                                                  (rel ~dir:srcDir
                                                     (libName ^
                                                        ("__" ^
                                                           ((String.uncapitalize
                                                               m)
                                                              ^ ".cmi"))))) in
                                      let outNameNoExtNoDir =
                                        libName ^
                                          ("__" ^
                                             (fileNameNoExtNoDir path
                                                ~suffix:".re")) in
                                      let outCmi =
                                        (rel ~dir:srcDir
                                           (outNameNoExtNoDir ^ ".cmi"))
                                          |> (tapp 1) in
                                      let outCmo =
                                        rel ~dir:srcDir
                                          (outNameNoExtNoDir ^ ".cmo") in
                                      [Rule.create ~targets:[outCmi; outCmo]
                                         ((Dep.all_unit ((Dep.path path) ::
                                             (Dep.path moduleAliasCmiPath) ::
                                             moduleDeps))
                                            *>>|
                                            (fun ()  ->
                                               bashf ~dir:srcDir
                                                 "ocamlc -pp refmt -g -open %s -I %s -o %s -intf-suffix .rei -c -impl %s"
                                                 (String.capitalize libName)
                                                 (ts srcDir)
                                                 outNameNoExtNoDir
                                                 (Path.basename path)));
                                      Rule.default ~dir
                                        [Dep.path outCmi; Dep.path outCmo]]) in
                            moduleAliasContentRules @
                              (moduleAliasCompileRules @ sourcesCompileRules)))))])
    else Scheme.no_rules
let env = Env.create scheme
let setup () = Deferred.return env
