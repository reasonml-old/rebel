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
let stdlibModules = ["List"; "String"; "Set"; "Queue"; "Printf"; "Stack"]
let fileNameNoExtNoDir path ~suffix  =
  (Path.basename path) |> (String.chop_suffix_exn ~suffix)
let _ = fileNameNoExtNoDir
let sortPathsTopologically ~buildDirRoot  ~libName  ~dir  ~paths  =
  ignore buildDirRoot;
  ignore libName;
  (Dep.action_stdout
     ((Dep.all_unit (List.map paths ~f:Dep.path)) *>>|
        (fun ()  ->
           let pathsString =
             (List.map (taplp 1 paths)
                ~f:(fun a  -> " -impl " ^ (Path.basename a)))
               |> (String.concat ~sep:" ") in
           bashf ~dir "ocamldep -pp refmt -sort -one-line %s"
             (tap 1 pathsString))))
    *>>|
    ((fun string  ->
        ((String.split string ~on:' ') |> (List.filter ~f:non_blank)) |>
          (List.map ~f:(rel ~dir))))
let _ = sortPathsTopologically
let parseOcamlDepModulesOutput ~dir  raw =
  (((String.strip (tap 1 raw)) |> (String.split ~on:'\n')) |>
     (List.filter ~f:non_blank))
    |>
    (List.map
       ~f:(fun line  ->
             match (String.strip line) |> (String.split ~on:':') with
             | path::deps::[] ->
                 ((rel ~dir path),
                   (((String.split deps ~on:' ') |>
                       (List.filter ~f:non_blank))
                      |>
                      (List.filter
                         ~f:(fun d  ->
                               not
                                 (List.exists stdlibModules
                                    ~f:(fun m  -> m = d))))))
             | _ ->
                 failwith "expected exactly one ':' in ocamldep output line"))
let _ = parseOcamlDepModulesOutput
let getDepModuleSingle ~dir  ~sourcePath  =
  (Dep.action_stdout
     ((Dep.path sourcePath) *>>|
        (fun ()  ->
           bashf ~dir "ocamldep -pp refmt -modules -one-line -impl %s"
             (Path.basename sourcePath))))
    *>>|
    (fun string  ->
       match (String.strip string) |> (String.split ~on:':') with
       | _::deps::[] ->
           ((String.split deps ~on:' ') |> (List.filter ~f:non_blank)) |>
             (List.filter
                ~f:(fun d  ->
                      not (List.exists stdlibModules ~f:(fun m  -> m = d))))
       | _ -> failwith "expected exactly one ':' in ocamldep output line")
let _ = getDepModuleSingle
let getDepModules ~dir  ~sourcePaths  =
  (Dep.all
     (List.map sourcePaths
        ~f:(fun path  -> getDepModuleSingle ~dir ~sourcePath:path)))
    *>>|
    (fun depsDeps  ->
       let string =
         (List.map2_exn sourcePaths depsDeps
            ~f:(fun path  ->
                  fun deps  ->
                    (ts path) ^
                      (":" ^ ((String.concat ~sep:" " deps) ^ "\n"))))
           |> (String.concat ~sep:"") in
       string)
let _ = getDepModules
let topLibName = "top"
let _ = topLibName
let topologicallySort ~mainNode  muhGraph =
  let rec topologicallySort' currNode muhGraph accum =
    let nodeDeps =
      match List.find muhGraph ~f:(fun (n,_)  -> n = currNode) with
      | None  -> raise ((Invalid_argument (currNode)))
      | ((Some ((_,nodeDeps)))) -> nodeDeps in
    List.iter nodeDeps ~f:(fun dep  -> topologicallySort' dep muhGraph accum);
    if not @@ (List.exists accum.contents ~f:(fun n  -> n = currNode))
    then accum := (currNode :: (accum.contents)) in
  let accum = { contents = [] } in
  topologicallySort' mainNode muhGraph accum;
  (List.rev accum.contents) |> (List.filter ~f:(fun m  -> m <> mainNode))
let _ = topologicallySort
let sortTransitiveThirdParties ~topLibName  ~nodeModulesRoot  ~buildDirRoot 
  =
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  (Dep.subdirs ~dir:nodeModulesRoot) *>>=
    ((fun thirdPartyNodeModulesRoots  ->
        let buildRoots = (rel ~dir:buildDirRoot topLibName) ::
          (List.map thirdPartyNodeModulesRoots
             ~f:(fun buildRoot  ->
                   rel ~dir:buildDirRoot (Path.basename buildRoot))) in
        let dependenciesFiles =
          List.map buildRoots
            ~f:(fun buildRoot  -> rel ~dir:buildRoot "dependencies") in
        (Dep.all (List.map dependenciesFiles ~f:Dep.contents)) *>>|
          (fun raws  ->
             let depsDeps =
               List.mapi raws
                 ~f:(fun i  ->
                       fun raw  ->
                         let buildRoot = List.nth_exn buildRoots i in
                         let assocList =
                           parseOcamlDepModulesOutput ~dir:buildRoot raw in
                         let (paths,allDeps) = List.unzip assocList in
                         let firstPartyModules =
                           List.map paths
                             ~f:(fun path  ->
                                   (fileNameNoExtNoDir ~suffix:".re" path) |>
                                     String.capitalize) in
                         ((List.concat allDeps) |> List.dedup) |>
                           (List.filter
                              ~f:(fun m  ->
                                    not
                                      (List.exists firstPartyModules
                                         ~f:(fun m'  -> m = m'))))) in
             topologicallySort ~mainNode:(String.capitalize topLibName)
               (List.zip_exn
                  (List.map buildRoots
                     ~f:(fun r  -> (Path.basename r) |> String.capitalize))
                  depsDeps))))
let _ = sortTransitiveThirdParties
let compileLibScheme ?(isTopLevelLib= true)  ~srcDir  ~libName  ~buildDir 
  ~nodeModulesRoot  ~buildDirRoot  =
  ignore isTopLevelLib;
  (let moduleAliasFilePath = rel ~dir:buildDir (libName ^ ".re") in
   let moduleAliasCmoPath = rel ~dir:buildDir (libName ^ ".cmo") in
   let moduleAliasCmiPath = rel ~dir:buildDir (libName ^ ".cmi") in
   let moduleAliasCmtPath = rel ~dir:buildDir (libName ^ ".cmt") in
   Scheme.dep
     ((Dep.glob_listing (Glob.create ~dir:srcDir "*.re")) *>>=
        (fun unsortedPaths  ->
           ignore 1;
           (sortPathsTopologically ~buildDirRoot ~libName ~dir:srcDir
              ~paths:unsortedPaths)
             *>>=
             ((fun sortedPaths  ->
                 (getDepModules ~dir:srcDir ~sourcePaths:unsortedPaths) *>>|
                   (fun rawOutput  ->
                      let rawDepsOutputRules =
                        [Rule.simple
                           ~targets:[rel ~dir:buildDir "dependencies"]
                           ~deps:(List.map unsortedPaths ~f:Dep.path)
                           ~action:(Action.save rawOutput
                                      ~target:(rel ~dir:buildDir
                                                 "dependencies"))] in
                      let moduleAliasContent =
                        (List.map unsortedPaths
                           ~f:(fun path  ->
                                 let name =
                                   fileNameNoExtNoDir path ~suffix:".re" in
                                 Printf.sprintf "let module %s = %s__%s;\n"
                                   (String.capitalize name)
                                   (String.capitalize libName) name))
                          |> (String.concat ~sep:"") in
                      let moduleAliasContentRules =
                        [Rule.create ~targets:[moduleAliasFilePath]
                           (Dep.return
                              (Action.save moduleAliasContent
                                 ~target:moduleAliasFilePath))] in
                      let moduleAliasCompileRules =
                        [Rule.create
                           ~targets:[moduleAliasCmoPath;
                                    tapp 1 moduleAliasCmiPath;
                                    moduleAliasCmtPath]
                           ((Dep.path moduleAliasFilePath) *>>|
                              (fun ()  ->
                                 bashf ~dir:buildDir
                                   "ocamlc -pp refmt -bin-annot -g -no-alias-deps -w -49 -c -impl %s -o %s"
                                   (Path.basename moduleAliasFilePath)
                                   (Path.basename moduleAliasCmoPath)))] in
                      let sourcesCompileRules =
                        Scheme.rules_dep @@
                          (Dep.all @@
                             (List.map unsortedPaths
                                ~f:(fun path  ->
                                      (getDepModuleSingle ~dir:srcDir
                                         ~sourcePath:path)
                                        *>>|
                                        (fun modules  ->
                                           let firstPartyModules =
                                             List.filter (tapl 1 modules)
                                               ~f:(fun m  ->
                                                     List.exists
                                                       unsortedPaths
                                                       ~f:(fun path  ->
                                                             ((fileNameNoExtNoDir
                                                                 ~suffix:".re"
                                                                 path)
                                                                |>
                                                                String.capitalize)
                                                               = m)) in
                                           let thirdPartyModules =
                                             List.filter modules
                                               ~f:(fun m  ->
                                                     not
                                                       (List.exists
                                                          unsortedPaths
                                                          ~f:(fun path  ->
                                                                ((fileNameNoExtNoDir
                                                                    ~suffix:".re"
                                                                    path)
                                                                   |>
                                                                   String.capitalize)
                                                                  = m))) in
                                           let firstPartyModuleDeps =
                                             List.map
                                               (tapl 1 firstPartyModules)
                                               ~f:(fun m  ->
                                                     Dep.path
                                                       (rel ~dir:buildDir
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
                                             (rel ~dir:buildDir
                                                (outNameNoExtNoDir ^ ".cmi"))
                                               |> (tapp 1) in
                                           let outCmo =
                                             rel ~dir:buildDir
                                               (outNameNoExtNoDir ^ ".cmo") in
                                           let outCmt =
                                             rel ~dir:buildDir
                                               (outNameNoExtNoDir ^ ".cmt") in
                                           Rule.create
                                             ~targets:[outCmi;
                                                      outCmo;
                                                      outCmt]
                                             ((Dep.all_unit
                                                 ([Dep.path path;
                                                  Dep.path moduleAliasCmiPath;
                                                  Dep.path moduleAliasCmoPath;
                                                  Dep.path moduleAliasCmtPath;
                                                  Dep.path
                                                    moduleAliasFilePath] @
                                                    (firstPartyModuleDeps @
                                                       [Dep.all_unit
                                                          (List.map
                                                             (tapl 1
                                                                thirdPartyModules)
                                                             ~f:(fun m  ->
                                                                   let libName
                                                                    =
                                                                    String.uncapitalize
                                                                    m in
                                                                   (Dep.glob_listing
                                                                    (Glob.create
                                                                    ~dir:(
                                                                    rel
                                                                    ~dir:(
                                                                    rel
                                                                    ~dir:nodeModulesRoot
                                                                    libName)
                                                                    "src")
                                                                    "*.re"))
                                                                    *>>=
                                                                    (fun
                                                                    sources 
                                                                    ->
                                                                    Dep.all_unit
                                                                    (List.map
                                                                    sources
                                                                    ~f:(
                                                                    fun
                                                                    sourcePath
                                                                     ->
                                                                    Dep.path
                                                                    (rel
                                                                    ~dir:(
                                                                    rel
                                                                    ~dir:buildDirRoot
                                                                    libName)
                                                                    (libName
                                                                    ^
                                                                    ("__" ^
                                                                    ((fileNameNoExtNoDir
                                                                    ~suffix:".re"
                                                                    sourcePath)
                                                                    ^ ".cmi")))))))))])))
                                                *>>|
                                                (fun ()  ->
                                                   bashf ~dir:buildDir
                                                     "ocamlc -pp refmt -bin-annot -g -open %s -I %s %s -o %s -intf-suffix rei -c -impl %s"
                                                     (String.capitalize
                                                        libName)
                                                     (ts buildDir)
                                                     ((List.map
                                                         thirdPartyModules
                                                         ~f:(fun m  ->
                                                               "-I " ^
                                                                 (((String.uncapitalize
                                                                    m) |>
                                                                    (rel
                                                                    ~dir:buildDirRoot))
                                                                    |>
                                                                    (
                                                                    Path.reach_from
                                                                    ~dir:buildDir))))
                                                        |>
                                                        (String.concat
                                                           ~sep:" "))
                                                     outNameNoExtNoDir
                                                     (Path.reach_from
                                                        ~dir:buildDir path))))))) in
                      let cmos =
                        List.map sortedPaths
                          ~f:(fun path  ->
                                let outNameNoExtNoDir =
                                  libName ^
                                    ("__" ^
                                       (fileNameNoExtNoDir path ~suffix:".re")) in
                                rel ~dir:buildDir
                                  (outNameNoExtNoDir ^ ".cmo")) in
                      let cmaPath = rel ~dir:buildDir "lib.cma" in
                      let cmaCompileRulesScheme =
                        if isTopLevelLib
                        then
                          Scheme.dep @@
                            ((sortTransitiveThirdParties ~topLibName:libName
                                ~nodeModulesRoot ~buildDirRoot)
                               *>>|
                               (fun thirdPartyTransitiveFuckingModules  ->
                                  let transitiveCmaPaths =
                                    List.map
                                      thirdPartyTransitiveFuckingModules
                                      ~f:(fun t  ->
                                            rel
                                              ~dir:(rel ~dir:buildDirRoot
                                                      (String.uncapitalize t))
                                              "lib.cma") in
                                  Scheme.rules
                                    [Rule.simple
                                       ~targets:[rel ~dir:buildDir "lib.cma"]
                                       ~deps:([Dep.path moduleAliasCmoPath] @
                                                ((List.map cmos ~f:Dep.path)
                                                   @
                                                   (List.map
                                                      transitiveCmaPaths
                                                      ~f:Dep.path)))
                                       ~action:(bashf ~dir:buildDir
                                                  "ocamlc -g -open %s -a -o %s %s %s %s"
                                                  (String.capitalize libName)
                                                  (Path.basename cmaPath)
                                                  (((taplp 1
                                                       transitiveCmaPaths)
                                                      |>
                                                      (List.map
                                                         ~f:(Path.reach_from
                                                               ~dir:buildDir)))
                                                     |>
                                                     (String.concat ~sep:" "))
                                                  (Path.basename
                                                     moduleAliasCmoPath)
                                                  ((List.map cmos
                                                      ~f:Path.basename)
                                                     |>
                                                     (String.concat ~sep:" ")))]))
                        else
                          Scheme.rules
                            [Rule.simple
                               ~targets:[rel ~dir:buildDir "lib.cma"]
                               ~deps:((Dep.path moduleAliasCmoPath) ::
                               (List.map cmos ~f:Dep.path))
                               ~action:(bashf ~dir:buildDir
                                          "ocamlc -g -open %s -a -o %s %s %s"
                                          (String.capitalize libName)
                                          (Path.basename cmaPath)
                                          (Path.basename moduleAliasCmoPath)
                                          ((List.map cmos ~f:Path.basename)
                                             |> (String.concat ~sep:" ")))] in
                      let finalOutputRules =
                        if isTopLevelLib
                        then
                          let topOutputPath = rel ~dir:buildDir "output.out" in
                          [Rule.simple ~targets:[topOutputPath]
                             ~deps:[Dep.path cmaPath;
                                   Dep.path
                                     (rel ~dir:buildDir
                                        (topLibName ^ "__main.cmo"))]
                             ~action:(bashf ~dir:buildDir
                                        "ocamlc -g -o %s %s %s"
                                        (Path.basename topOutputPath)
                                        (Path.basename cmaPath)
                                        (topLibName ^ "__main.cmo"))]
                        else [] in
                      Scheme.all
                        [Scheme.rules
                           (rawDepsOutputRules @
                              (moduleAliasContentRules @
                                 (moduleAliasCompileRules @ finalOutputRules)));
                        sourcesCompileRules;
                        cmaCompileRulesScheme]))))))
let generateDotMerlinScheme ~nodeModulesRoot  ~buildDirRoot  ~isTopLevelLib 
  ~libName  ~dir  ~root  =
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  ignore isTopLevelLib;
  ignore libName;
  ignore dir;
  ignore root;
  (let dotMerlinContent =
     Printf.sprintf {|%s
S %s

B %s

FLG -w -30 -w -40 -open %s
|}
       (match isTopLevelLib with | true  -> "src/*" | false  -> "")
       (Path.reach_from ~dir (rel ~dir:nodeModulesRoot "**/src"))
       (Path.reach_from ~dir (rel ~dir:buildDirRoot "**"))
       (String.capitalize libName) in
   Scheme.rules
     [Rule.simple ~targets:[rel ~dir ".merlin"] ~deps:[]
        ~action:(Action.save dotMerlinContent ~target:(rel ~dir ".merlin"))])
let scheme ~dir  =
  let nodeModulesRoot = Path.root_relative "node_modules" in
  let buildDirRoot = Path.root_relative "_build" in
  ignore dir;
  ignore buildDirRoot;
  ignore nodeModulesRoot;
  if dir = root
  then
    (let dotMerlinScheme =
       Scheme.rules_dep
         ((Dep.subdirs ~dir:nodeModulesRoot) *>>|
            (fun thirdPartyNodeModulesRoots  ->
               List.map thirdPartyNodeModulesRoots
                 ~f:(fun path  ->
                       Rule.default ~dir [Dep.path (rel ~dir:path ".merlin")]))) in
     let dotMerlinGenScheme =
       generateDotMerlinScheme ~buildDirRoot ~isTopLevelLib:true
         ~nodeModulesRoot ~dir ~root ~libName:topLibName in
     Scheme.all
       [dotMerlinGenScheme;
       Scheme.rules
         [Rule.default ~dir
            [Dep.path
               (rel ~dir:(rel ~dir:buildDirRoot topLibName) "output.out");
            Dep.path (rel ~dir:root ".merlin")]];
       dotMerlinScheme])
  else
    if Path.is_descendant ~dir:(rel ~dir:root "_build") dir
    then
      (let libName = Path.basename dir in
       let srcDir =
         if libName = topLibName
         then rel ~dir:root "src"
         else rel ~dir:(rel ~dir:nodeModulesRoot libName) "src" in
       compileLibScheme ~srcDir ~isTopLevelLib:(libName = topLibName)
         ~libName ~buildDir:(rel ~dir:buildDirRoot libName) ~buildDirRoot
         ~nodeModulesRoot)
    else
      if (Path.dirname dir) = nodeModulesRoot
      then
        (let libName = Path.basename dir in
         generateDotMerlinScheme ~buildDirRoot ~isTopLevelLib:false
           ~nodeModulesRoot ~dir ~root ~libName)
      else Scheme.no_rules
let env = Env.create scheme
let setup () = Deferred.return env
