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
let fileNameNoExtNoDir path ~suffix  =
  (Path.basename path) |> (String.chop_suffix_exn ~suffix)
let _ = fileNameNoExtNoDir
let sortPathsTopologically ~dir  ~paths  =
  (Dep.action_stdout
     ((Dep.all_unit (List.map paths ~f:Dep.path)) *>>|
        (fun ()  ->
           let pathsString =
             (List.map paths ~f:(fun a  -> " -impl " ^ (Path.basename a))) |>
               (String.concat ~sep:" ") in
           bashf ~dir "ocamldep -pp refmt -sort -one-line %s" pathsString)))
    *>>|
    (fun string  ->
       ((String.split string ~on:' ') |> (List.filter ~f:non_blank)) |>
         (List.map ~f:(rel ~dir)))
let _ = sortPathsTopologically
let getDepModules ~dir  ~sourcePaths  =
  (Dep.action_stdout
     ((Dep.all_unit (List.map sourcePaths ~f:Dep.path)) *>>|
        (fun ()  ->
           bashf ~dir "ocamldep -pp refmt -modules -one-line %s"
             (((List.map (taplp 1 sourcePaths)
                  ~f:(fun a  -> " -impl " ^ (Path.basename a)))
                 |> (String.concat ~sep:""))
                |> (tap 1)))))
    *>>|
    (fun string  ->
       (((String.strip (tap 1 string)) |> (String.split ~on:'\n')) |>
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
let topLibName = "hi"
let _ = topLibName
let compileLib ?(isTopLevelLib= true)  ~srcDir  ~libName  ~buildDir 
  ~buildDirRoot  =
  ignore isTopLevelLib;
  (let moduleAliasFilePath = rel ~dir:buildDir (libName ^ ".re") in
   let moduleAliasCmoPath = rel ~dir:buildDir (libName ^ ".cmo") in
   let moduleAliasCmiPath = rel ~dir:buildDir (libName ^ ".cmi") in
   Scheme.rules_dep
     ((Dep.glob_listing (Glob.create ~dir:srcDir "*.re")) *>>=
        (fun unsortedPaths  ->
           let filteredUnsortedPaths = unsortedPaths in
           (sortPathsTopologically ~dir:srcDir ~paths:filteredUnsortedPaths)
             *>>=
             (fun sortedPaths  ->
                (getDepModules ~dir:srcDir ~sourcePaths:sortedPaths) *>>|
                  (fun assocList  ->
                     let moduleAliasContent =
                       (List.map filteredUnsortedPaths
                          ~f:(fun path  ->
                                let name =
                                  fileNameNoExtNoDir path ~suffix:".re" in
                                Printf.sprintf "let module %s = %s__%s;"
                                  (String.capitalize name)
                                  (String.capitalize libName) name))
                         |> (String.concat ~sep:"\n") in
                     let moduleAliasContentRules =
                       [Rule.create ~targets:[moduleAliasFilePath]
                          ((Dep.all_unit
                              (List.map filteredUnsortedPaths ~f:Dep.path))
                             *>>|
                             (fun ()  ->
                                bashf ~dir:buildDir "echo %s > %s"
                                  (Shell.escape moduleAliasContent)
                                  (Path.basename moduleAliasFilePath)));
                       Rule.default ~dir:buildDir
                         [Dep.path moduleAliasFilePath]] in
                     let moduleAliasCompileRules =
                       [Rule.create
                          ~targets:[moduleAliasCmoPath;
                                   tapp 1 moduleAliasCmiPath]
                          ((Dep.path moduleAliasFilePath) *>>|
                             (fun ()  ->
                                bashf ~dir:buildDir
                                  "ocamlc -pp refmt -g -no-alias-deps -w -49 -c -impl %s -o %s"
                                  (Path.basename moduleAliasFilePath)
                                  (Path.basename moduleAliasCmoPath)));
                       Rule.default ~dir:buildDir
                         [Dep.path moduleAliasCmoPath;
                         Dep.path moduleAliasCmiPath]] in
                     let sourcesCompileRules =
                       List.concat_map sortedPaths
                         ~f:(fun path  ->
                               let modules =
                                 match List.Assoc.find assocList path with
                                 | None  -> failwith ("lookup: " ^ (ts path))
                                 | ((Some (modules))) ->
                                     modules in
                               let firstPartyModules =
                                 List.filter (tapl 1 modules)
                                   ~f:(fun m  ->
                                         List.exists sortedPaths
                                           ~f:(fun path  ->
                                                 ((fileNameNoExtNoDir
                                                     ~suffix:".re" path)
                                                    |> String.capitalize)
                                                   = m)) in
                               let thirdPartyModules =
                                 List.filter modules
                                   ~f:(fun m  ->
                                         not
                                           (List.exists sortedPaths
                                              ~f:(fun path  ->
                                                    ((fileNameNoExtNoDir
                                                        ~suffix:".re" path)
                                                       |> String.capitalize)
                                                      = m))) in
                               let firstPartyModuleDeps =
                                 List.map (tapl 1 firstPartyModules)
                                   ~f:(fun m  ->
                                         Dep.path
                                           (rel ~dir:buildDir
                                              (libName ^
                                                 ("__" ^
                                                    ((String.uncapitalize m)
                                                       ^ ".cmi"))))) in
                               let outNameNoExtNoDir =
                                 libName ^
                                   ("__" ^
                                      (fileNameNoExtNoDir path ~suffix:".re")) in
                               let outCmi =
                                 (rel ~dir:buildDir
                                    (outNameNoExtNoDir ^ ".cmi"))
                                   |> (tapp 1) in
                               let outCmo =
                                 rel ~dir:buildDir
                                   (outNameNoExtNoDir ^ ".cmo") in
                               [Rule.create ~targets:[outCmi; outCmo]
                                  ((Dep.all_unit ((Dep.path path) ::
                                      (Dep.path moduleAliasCmiPath) ::
                                      (Dep.path moduleAliasCmoPath) ::
                                      (Dep.path moduleAliasFilePath) ::
                                      firstPartyModuleDeps))
                                     *>>|
                                     (fun ()  ->
                                        bashf ~dir:buildDir
                                          "ocamlc -pp refmt -g -open %s -I %s %s -o %s -intf-suffix rei -c -impl %s"
                                          (String.capitalize libName)
                                          (ts buildDir)
                                          ((List.map thirdPartyModules
                                              ~f:(fun m  ->
                                                    "-I " ^
                                                      (((String.uncapitalize
                                                           m)
                                                          |>
                                                          (rel
                                                             ~dir:buildDirRoot))
                                                         |>
                                                         (Path.reach_from
                                                            ~dir:buildDir))))
                                             |> (String.concat ~sep:" "))
                                          outNameNoExtNoDir
                                          (Path.reach_from ~dir:buildDir path)));
                               Rule.default ~dir:buildDir
                                 [Dep.path outCmi; Dep.path outCmo]]) in
                     let cmos =
                       List.map sortedPaths
                         ~f:(fun path  ->
                               let outNameNoExtNoDir =
                                 libName ^
                                   ("__" ^
                                      (fileNameNoExtNoDir path ~suffix:".re")) in
                               rel ~dir:buildDir (outNameNoExtNoDir ^ ".cmo")) in
                     let cmaPath = rel ~dir:buildDir "lib.cma" in
                     let cmaCompileRules =
                       [Rule.simple ~targets:[rel ~dir:buildDir "lib.cma"]
                          ~deps:((Dep.path moduleAliasCmoPath) ::
                          (List.map cmos ~f:Dep.path))
                          ~action:(bashf ~dir:buildDir
                                     "ocamlc -g -open %s -a -o %s %s %s"
                                     (String.capitalize libName)
                                     (Path.basename cmaPath)
                                     (Path.basename moduleAliasCmoPath)
                                     ((List.map cmos ~f:Path.basename) |>
                                        (String.concat ~sep:" ")));
                       Rule.default ~dir:buildDir [Dep.path cmaPath]] in
                     let finalOutputRules =
                       if isTopLevelLib
                       then
                         let topOutputPath = rel ~dir:buildDir "output.out" in
                         [Rule.simple ~targets:[topOutputPath]
                            ~deps:[Dep.path cmaPath;
                                  Dep.path (rel ~dir:buildDir "hi__main.cmo")]
                            ~action:(bashf ~dir:buildDir
                                       "ocamlc -g -o %s %s %s"
                                       (Path.basename topOutputPath)
                                       (Path.basename cmaPath) "hi__main.cmo");
                         Rule.default ~dir:buildDir [Dep.path topOutputPath]]
                       else [] in
                     moduleAliasContentRules @
                       (moduleAliasCompileRules @
                          (sourcesCompileRules @
                             (cmaCompileRules @ finalOutputRules))))))))
let scheme ~dir  =
  let nodeModulesRoot = Path.root_relative "node_modules" in
  let buildDirRoot = Path.root_relative "_build" in
  let srcDirChenglou = Path.root_relative "node_modules/chenglou/src" in
  let buildDirChenglou = Path.root_relative "_build/chenglou" in
  let libNameChenglou = "chenglou" in
  let srcDirHi = Path.root_relative "src" in
  let buildDirHi = Path.root_relative "_build/hi" in
  let libNameHi = "hi" in
  ignore dir;
  ignore buildDirRoot;
  ignore nodeModulesRoot;
  ignore srcDirChenglou;
  ignore buildDirChenglou;
  ignore libNameChenglou;
  ignore srcDirHi;
  ignore buildDirHi;
  ignore libNameHi;
  print_endline @@ ((ts dir) ^ "<<<<<<<<<<<<<<<");
  if (dir = root) || ((Path.basename dir) = "src")
  then
    Scheme.all
      [Scheme.rules
         [Rule.default ~dir [Dep.path (rel ~dir:buildDirHi "output.out")]];
      Scheme.dep
        ((Dep.subdirs ~dir:nodeModulesRoot) *>>|
           (fun thirdPartyDepsRoots  ->
              let a = 1 in
              ignore a;
              Scheme.all
                (List.map thirdPartyDepsRoots
                   ~f:(fun depRoot  ->
                         let libName = Path.basename depRoot in
                         let buildDir = rel ~dir:buildDirRoot libName in
                         Scheme.rules
                           [Rule.default ~dir
                              [Dep.path (rel ~dir:buildDir "lib.cma")]]))))]
  else
    if Path.is_descendant ~dir:(rel ~dir:root "_build") dir
    then
      (let libName = Path.basename dir in
       let srcDir =
         if libName = topLibName
         then rel ~dir:root "src"
         else rel ~dir:(rel ~dir:nodeModulesRoot libName) "src" in
       Scheme.all
         [compileLib ~srcDir ~isTopLevelLib:true ~libName
            ~buildDir:(rel ~dir:buildDirRoot libName) ~buildDirRoot])
    else Scheme.no_rules
let env = Env.create scheme
let setup () = Deferred.return env
