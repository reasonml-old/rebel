open Core.Std
open Async.Std
open Jenga_lib.Api
let ( *>>| ) = Dep.map
let ( *>>= ) = Dep.bind
let rel = Path.relative
let ts = Path.to_string
let root = Path.the_root
let bash ~dir  command =
  Action.process ~dir ~prog:"bash" ~args:["-c"; command] ()
let bashf ~dir  fmt = ksprintf (fun str  -> bash ~dir str) fmt
let nonBlank s = match String.strip s with | "" -> false | _ -> true
let chopSuffixExn str = String.slice str 0 (String.rindex_exn str '.')
let fileNameNoExtNoDir path = (Path.basename path) |> chopSuffixExn
let _ = fileNameNoExtNoDir
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
let tapAssocList n a =
  if n = 0
  then
    (print_endline "tapAssocList---------";
     List.iter a
       ~f:(fun (path,deps)  ->
             print_endline @@ (path ^ (": " ^ (String.concat ~sep:" " deps))));
     a)
  else a
let _ = tapAssocList
let topLibName = "top"
let finalOutputName = "app"
let libraryFileName = "lib.cma"
let nodeModulesRoot = rel ~dir:root "node_modules"
let buildDirRoot = rel ~dir:root "_build"
let topSrcDir = rel ~dir:root "src"
let stringCapitalize a = a
let stringUncapitalize a = a
let _ = stringCapitalize
let _ = stringUncapitalize
let ocamlDepModules ~sourcePath  =
  (Dep.subdirs ~dir:nodeModulesRoot) *>>=
    (fun subdirs  ->
       let execDir = Path.dirname sourcePath in
       let thirdPartyBuildRoots =
         List.map subdirs
           ~f:(fun subdir  -> rel ~dir:buildDirRoot (Path.basename subdir)) in
       (Dep.action_stdout
          ((Dep.all_unit
              [Dep.path sourcePath;
              Dep.all_unit
                (List.map thirdPartyBuildRoots
                   ~f:(fun buildRoot  ->
                         (rel ~dir:buildRoot
                            ((Path.basename buildRoot) ^ ".cmi"))
                           |> Dep.path))])
             *>>|
             (fun ()  ->
                bashf ~dir:execDir
                  "ocamldep -pp refmt %s -ml-synonym .re -mli-synonym .rei -one-line %s"
                  (((thirdPartyBuildRoots |>
                       (List.map
                          ~f:(fun path  -> Path.reach_from ~dir:execDir path)))
                      |> (List.map ~f:(fun path  -> "-I " ^ path)))
                     |> (String.concat ~sep:" ")) (Path.basename sourcePath))))
         *>>|
         (fun string  ->
            match (((String.strip string) |> (String.split ~on:'\n')) |>
                     List.hd_exn)
                    |> (String.split ~on:':')
            with
            | original::deps::[] ->
                (((((String.split deps ~on:' ') |> (List.filter ~f:nonBlank))
                     |> (List.map ~f:chopSuffixExn))
                    |>
                    (List.filter ~f:(fun m  -> m <> (chopSuffixExn original))))
                   |>
                   (List.map
                      ~f:(fun m  ->
                            match String.rindex m '/' with
                            | None  -> m
                            | ((Some (idx))) ->
                                (String.slice m (idx + 1) (String.length m))
                                  |> String.capitalize)))
                  |> (tapl 1)
            | _ ->
                failwith "expected exactly one ':' in ocamldep output line"))
let _ = ocamlDepModules
let getThirdPartyDepsForLib ~srcDir  =
  (Dep.glob_listing (Glob.create ~dir:srcDir "*.re")) *>>=
    (fun sourcePaths  ->
       (Dep.all
          (List.map sourcePaths
             ~f:(fun sourcePath  -> ocamlDepModules ~sourcePath)))
         *>>|
         (fun sourcePathsDeps  ->
            let internalDeps =
              List.map sourcePaths
                ~f:(fun path  ->
                      (fileNameNoExtNoDir path) |> stringCapitalize) in
            ((List.concat sourcePathsDeps) |> List.dedup) |>
              (List.filter
                 ~f:(fun dep  ->
                       not
                         (List.exists internalDeps
                            ~f:(fun dep'  -> dep = dep'))))))
let _ = getThirdPartyDepsForLib
let topologicalSort ~mainNode  assocListGraph =
  ignore @@ (tapAssocList 1 assocListGraph);
  (let rec topologicalSort' currNode assocListGraph accum =
     let nodeDeps =
       match List.find assocListGraph ~f:(fun (n,_)  -> n = currNode) with
       | None  -> []
       | ((Some ((_,nodeDeps')))) -> nodeDeps' in
     List.iter nodeDeps
       ~f:(fun dep  -> topologicalSort' dep assocListGraph accum);
     if not @@ (List.exists accum.contents ~f:(fun n  -> n = currNode))
     then accum := (currNode :: (accum.contents)) in
   let accum = { contents = [] } in
   topologicalSort' mainNode assocListGraph accum; List.rev accum.contents)
let _ = topologicalSort
let sortTransitiveThirdParties ~topLibName  =
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  (getThirdPartyDepsForLib ~srcDir:topSrcDir) *>>=
    ((fun topThirdPartyDeps  ->
        let thirdPartiesSrcDirs =
          List.map topThirdPartyDeps
            ~f:(fun dep  ->
                  rel
                    ~dir:(rel ~dir:nodeModulesRoot (String.uncapitalize dep))
                    "src") in
        let thirdPartiesThirdPartyDepsD =
          Dep.all
            (List.map thirdPartiesSrcDirs
               ~f:(fun srcDir  -> getThirdPartyDepsForLib ~srcDir)) in
        let topLibModuleName = String.capitalize topLibName in
        thirdPartiesThirdPartyDepsD *>>|
          (fun thirdPartiesThirdPartyDeps  ->
             ((List.zip_exn (topLibModuleName :: topThirdPartyDeps)
                 (topThirdPartyDeps :: thirdPartiesThirdPartyDeps))
                |> (topologicalSort ~mainNode:topLibModuleName))
               |> (List.filter ~f:(fun m  -> m <> topLibModuleName)))))
let sortPathsTopologically ~buildDirRoot  ~libName  ~dir  ~paths  =
  ignore buildDirRoot;
  ignore libName;
  (Dep.action_stdout
     ((Dep.all_unit (List.map paths ~f:Dep.path)) *>>|
        (fun ()  ->
           let pathsString =
             (List.map (taplp 1 paths) ~f:Path.basename) |>
               (String.concat ~sep:" ") in
           bashf ~dir
             "ocamldep -pp refmt -ml-synonym .re -mli-synonym .rei -sort -one-line %s"
             (tap 1 pathsString))))
    *>>|
    ((fun string  ->
        (((String.split string ~on:' ') |> (List.filter ~f:nonBlank)) |>
           (List.map ~f:(rel ~dir)))
          |> (taplp 1)))
let _ = sortPathsTopologically
let moduleAliasFileScheme ~buildDir  ~sourceModules  ~libName  =
  let name extension = rel ~dir:buildDir (libName ^ ("." ^ extension)) in
  let sourcePath = name "re" in
  let cmo = name "cmo" in
  let cmi = name "cmi" in
  let cmt = name "cmt" in
  let fileContent =
    (List.map sourceModules
       ~f:(fun moduleName  ->
             Printf.sprintf "let module %s = %s__%s;\n" moduleName
               (String.capitalize libName) moduleName))
      |> (String.concat ~sep:"") in
  let contentRule =
    Rule.create ~targets:[sourcePath]
      (Dep.return (Action.save fileContent ~target:sourcePath)) in
  let compileRule =
    Rule.create ~targets:[cmo; cmi; cmt]
      ((Dep.path sourcePath) *>>|
         (fun ()  ->
            bashf ~dir:buildDir
              "ocamlc -pp refmt -bin-annot -g -no-alias-deps -w -49 -w -30 -w -40 -c -impl %s -o %s"
              (Path.basename sourcePath) (Path.basename cmo))) in
  Scheme.rules [contentRule; compileRule]
let jsooLocationD =
  (Dep.action_stdout
     (Dep.return (bash ~dir:root "ocamlfind query js_of_ocaml")))
    *>>| String.strip
let compileCmaScheme ~sortedSourcePaths  ~libName  ~isTopLevelLib  ~buildDir 
  =
  let cmaPath = rel ~dir:buildDir libraryFileName in
  let moduleAliasCmoPath = rel ~dir:buildDir (libName ^ ".cmo") in
  let cmos =
    List.map sortedSourcePaths
      ~f:(fun path  ->
            rel ~dir:buildDir
              (libName ^ ("__" ^ ((fileNameNoExtNoDir path) ^ ".cmo")))) in
  let cmosString =
    (List.map cmos ~f:Path.basename) |> (String.concat ~sep:" ") in
  if isTopLevelLib
  then
    Scheme.dep
      ((Dep.both jsooLocationD
          (sortTransitiveThirdParties ~topLibName:libName))
         *>>|
         (fun (jsooLocation,thirdPartyTransitiveDeps)  ->
            let transitiveCmaPaths =
              List.map thirdPartyTransitiveDeps
                ~f:(fun dep  ->
                      rel
                        ~dir:(rel ~dir:buildDirRoot (String.uncapitalize dep))
                        libraryFileName) in
            Scheme.rules
              [Rule.simple ~targets:[cmaPath]
                 ~deps:(([moduleAliasCmoPath] @ (cmos @ transitiveCmaPaths))
                          |> (List.map ~f:Dep.path))
                 ~action:(bashf ~dir:buildDir
                            "ocamlc -g -I %s %s/js_of_ocaml.cma -open %s -a -o %s %s %s %s"
                            jsooLocation jsooLocation
                            (String.capitalize libName)
                            (Path.basename cmaPath)
                            ((transitiveCmaPaths |>
                                (List.map ~f:(Path.reach_from ~dir:buildDir)))
                               |> (String.concat ~sep:" "))
                            (Path.basename moduleAliasCmoPath) cmosString)]))
  else
    Scheme.rules
      [Rule.simple ~targets:[cmaPath]
         ~deps:(List.map (moduleAliasCmoPath :: cmos) ~f:Dep.path)
         ~action:(bashf ~dir:buildDir "ocamlc -g -open %s -a -o %s %s %s"
                    (String.capitalize libName) (Path.basename cmaPath)
                    (Path.basename moduleAliasCmoPath) cmosString)]
let finalOutputsScheme ~topBuildDir  =
  let cmaPath = rel ~dir:topBuildDir libraryFileName in
  let binaryPath = rel ~dir:topBuildDir (finalOutputName ^ ".out") in
  let jsooPath = rel ~dir:topBuildDir (finalOutputName ^ ".js") in
  Scheme.rules
    [Rule.simple ~targets:[binaryPath]
       ~deps:[Dep.path cmaPath;
             Dep.path (rel ~dir:topBuildDir (topLibName ^ "__Index.cmo"))]
       ~action:(bashf ~dir:topBuildDir "ocamlc -g -o %s %s %s"
                  (Path.basename binaryPath) (Path.basename cmaPath)
                  (topLibName ^ "__Index.cmo"));
    Rule.simple ~targets:[jsooPath] ~deps:[Dep.path binaryPath]
      ~action:(bashf ~dir:topBuildDir
                 "js_of_ocaml --source-map --no-inline --debug-info --pretty --linkall %s"
                 (Path.basename binaryPath))]
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
             *>>|
             ((fun sortedPaths  ->
                 let sourcesCompileRules =
                   Scheme.rules_dep @@
                     (Dep.all @@
                        (List.map unsortedPaths
                           ~f:(fun path  ->
                                 (ocamlDepModules ~sourcePath:path) *>>|
                                   (fun modules  ->
                                      let firstPartyModules =
                                        List.filter (tapl 1 modules)
                                          ~f:(fun m  ->
                                                List.exists unsortedPaths
                                                  ~f:(fun path  ->
                                                        ((fileNameNoExtNoDir
                                                            path)
                                                           |>
                                                           stringCapitalize)
                                                          = m)) in
                                      let thirdPartyModules =
                                        List.filter modules
                                          ~f:(fun m  ->
                                                not
                                                  (List.exists unsortedPaths
                                                     ~f:(fun path  ->
                                                           ((fileNameNoExtNoDir
                                                               path)
                                                              |>
                                                              stringCapitalize)
                                                             = m))) in
                                      let firstPartyModuleDeps =
                                        List.map (tapl 1 firstPartyModules)
                                          ~f:(fun m  ->
                                                Dep.path
                                                  (rel ~dir:buildDir
                                                     (libName ^
                                                        ("__" ^
                                                           ((stringUncapitalize
                                                               m)
                                                              ^ ".cmi"))))) in
                                      let outNameNoExtNoDir =
                                        libName ^
                                          ("__" ^ (fileNameNoExtNoDir path)) in
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
                                        ~targets:[outCmi; outCmo; outCmt]
                                        ((Dep.all_unit
                                            ([Dep.path path;
                                             Dep.path moduleAliasCmiPath;
                                             Dep.path moduleAliasCmoPath;
                                             Dep.path moduleAliasCmtPath;
                                             Dep.path moduleAliasFilePath] @
                                               (firstPartyModuleDeps @
                                                  [Dep.all_unit
                                                     (List.map
                                                        (tapl 1
                                                           thirdPartyModules)
                                                        ~f:(fun m  ->
                                                              let libName =
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
                                                                (fun sources 
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
                                                                    sourcePath)
                                                                    ^ ".cmi")))))))))])))
                                           *>>|
                                           (fun ()  ->
                                              bashf ~dir:buildDir
                                                "ocamlc -pp refmt -bin-annot -g -w -30 -w -40 -open %s -I `ocamlfind query js_of_ocaml` `ocamlfind query js_of_ocaml`/js_of_ocaml.cma -I %s %s -o %s -intf-suffix rei -c -impl %s"
                                                (String.capitalize libName)
                                                (ts buildDir)
                                                ((List.map thirdPartyModules
                                                    ~f:(fun m  ->
                                                          "-I " ^
                                                            (((stringUncapitalize
                                                                 m)
                                                                |>
                                                                (rel
                                                                   ~dir:buildDirRoot))
                                                               |>
                                                               (Path.reach_from
                                                                  ~dir:buildDir))))
                                                   |>
                                                   (String.concat ~sep:" "))
                                                outNameNoExtNoDir
                                                (Path.reach_from
                                                   ~dir:buildDir path))))))) in
                 let finalOutputsScheme =
                   match isTopLevelLib with
                   | true  -> finalOutputsScheme ~topBuildDir:buildDir
                   | false  -> Scheme.no_rules in
                 Scheme.all
                   [moduleAliasFileScheme ~buildDir ~libName
                      ~sourceModules:(List.map unsortedPaths
                                        ~f:fileNameNoExtNoDir);
                   sourcesCompileRules;
                   compileCmaScheme ~buildDir ~isTopLevelLib ~libName
                     ~sortedSourcePaths:sortedPaths;
                   finalOutputsScheme])))))
let generateDotMerlinScheme ~nodeModulesRoot  ~buildDirRoot  ~isTopLevelLib 
  ~libName  ~dir  ~root  =
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  ignore isTopLevelLib;
  ignore libName;
  ignore dir;
  ignore root;
  (let dotMerlinContent =
     Printf.sprintf
       {|%s
S %s

B %s

PKG js_of_ocaml

FLG -w -30 -w -40 -open %s
|}
       (match isTopLevelLib with | true  -> "S src" | false  -> "")
       (Path.reach_from ~dir (rel ~dir:nodeModulesRoot "**/src"))
       (Path.reach_from ~dir (rel ~dir:buildDirRoot "*"))
       (String.capitalize libName) in
   Scheme.rules
     [Rule.simple ~targets:[rel ~dir ".merlin"] ~deps:[]
        ~action:(Action.save dotMerlinContent ~target:(rel ~dir ".merlin"))])
let scheme ~dir  =
  ignore dir;
  if dir = root
  then
    (let dotMerlinScheme =
       Scheme.rules_dep
         ((getThirdPartyDepsForLib ~srcDir:topSrcDir) *>>|
            (fun deps  ->
               let thirdPartyNodeModulesRoots =
                 List.map deps
                   ~f:(fun dep  ->
                         rel ~dir:nodeModulesRoot (String.uncapitalize dep)) in
               List.map thirdPartyNodeModulesRoots
                 ~f:(fun path  ->
                       Rule.default ~dir [Dep.path (rel ~dir:path ".merlin")]))) in
     Scheme.all
       [generateDotMerlinScheme ~buildDirRoot ~isTopLevelLib:true
          ~nodeModulesRoot ~dir ~root ~libName:topLibName;
       Scheme.rules
         [Rule.default ~dir
            [Dep.path
               (rel ~dir:(rel ~dir:buildDirRoot topLibName)
                  (finalOutputName ^ ".out"));
            Dep.path
              (rel ~dir:(rel ~dir:buildDirRoot topLibName)
                 (finalOutputName ^ ".js"));
            Dep.path (rel ~dir:root ".merlin")]];
       dotMerlinScheme])
  else
    if Path.is_descendant ~dir:buildDirRoot dir
    then
      (let libName = Path.basename dir in
       let srcDir =
         if libName = topLibName
         then topSrcDir
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
