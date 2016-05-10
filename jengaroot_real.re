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

let fileNameNoExtNoDir path suffix::suffix => Path.basename path |> String.chop_suffix_exn suffix::suffix;

fileNameNoExtNoDir;

let sortPathsTopologically dir::dir paths::paths =>
  /* List.map paths f::ts |> List.iter f::print_endline; */
  Dep.action_stdout (
    Dep.all_unit (
      /* Dep.glob_change (Glob.create dir::dir "*.re"),
         Dep.glob_change (Glob.create dir::dir "*.rei"), */
      List.map
        paths f::Dep.path
    )
      *>>| (
      fun () => {
        let pathsString =
          List.map (taplp 0 paths) f::(fun a => " -impl " ^ Path.basename a) |> String.concat sep::" ";
        bashf dir::dir "ocamldep -pp refmt -sort -one-line %s" (tap 1 pathsString)
      }
    )
  ) *>>| (
  fun string => String.split string on::' ' |> List.filter f::non_blank |> List.map f::(rel dir::dir)
);

sortPathsTopologically;

let parseOcamlDepModulesOutput dir::dir raw =>
  String.strip (tap 1 raw) |>
    String.split on::'\n' |>
    List.filter f::non_blank |>
    List.map
      f::(
        fun line =>
          switch (String.strip line |> String.split on::':') {
          | [path, deps] => (rel dir::dir path, String.split deps on::' ' |> List.filter f::non_blank)
          | _ => failwith "expected exactly one ':' in ocamldep output line"
          }
      );

parseOcamlDepModulesOutput;

let getDepModules dir::dir sourcePaths::sourcePaths =>
  Dep.action_stdout (
    Dep.all_unit (
      /* Dep.glob_change (Glob.create dir::dir "*.re"),
         Dep.glob_change (Glob.create dir::dir "*.rei"), */
      List.map
        sourcePaths f::Dep.path
    )
      *>>| (
      fun () =>
        bashf
          dir::dir
          "ocamldep -pp refmt -modules -one-line %s"
          (
            List.map (taplp 1 sourcePaths) f::(fun a => " -impl " ^ Path.basename a) |>
              String.concat sep::"" |> tap 1
          )
    )
  ) *>>| (
  fun string => (string, parseOcamlDepModulesOutput dir::dir string)
);

getDepModules;

let topLibName = "hi";

topLibName;

let topologicallySort firstNode::firstNode muhGraph => {
  /* print_endline "mahhhhhhhhhhhhhhhhhhhhhhh";
     List.iter
       muhGraph
       f::(
         fun (n, deps) => {
           print_string n;
           print_string ": ";
           List.iter deps f::print_endline;
           print_endline "---"
         }
       );
     print_endline "mahhhhhhhhhhhhhhhhhhhhhhh"; */
  let rec topologicallySort' currNode muhGraph accum => {
    let nodeDeps =
      switch (List.find muhGraph f::(fun (n, _) => n == currNode)) {
      | None => raise (Invalid_argument currNode)
      | Some (_, nodeDeps) => nodeDeps
      };
    /* if (nodeDeps == []) {
       } else { */
    List.iter nodeDeps f::(fun dep => topologicallySort' dep muhGraph accum);
    if (not @@ List.exists accum.contents f::(fun n => n == currNode)) {
      accum := [currNode, ...accum.contents]
    }
  };
  let accum = {contents: []};
  topologicallySort' firstNode muhGraph accum;
  /* print_endline "mahhhhhhhhhhhhhhhhhhhhhhh----------------";
     List.iter f::print_endline accum.contents;
     print_endline "mahhhhhhhhhhhhhhhhhhhhhhh----------------"; */
  List.rev accum.contents |> List.filter f::(fun m => m != firstNode)
};

topologicallySort;

let sortTransitiveThirdParties
    topLibName::topLibName
    nodeModulesRoot::nodeModulesRoot
    buildDirRoot::buildDirRoot => {
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  Dep.subdirs dir::nodeModulesRoot *>>= (
    fun thirdPartyNodeModulesRoots => {
      let buildRoots = [
        rel dir::buildDirRoot topLibName,
        ...List.map
             thirdPartyNodeModulesRoots f::(fun buildRoot => rel dir::buildDirRoot (Path.basename buildRoot))
      ];
      let dependenciesFiles = List.map buildRoots f::(fun buildRoot => rel dir::buildRoot "dependencies");
      Dep.all (List.map dependenciesFiles f::Dep.contents) *>>| (
        fun raws => {
          let depsDeps =
            List.mapi
              raws
              f::(
                fun i raw => {
                  let buildRoot = List.nth_exn buildRoots i;
                  let assocList = parseOcamlDepModulesOutput dir::buildRoot raw;
                  let (paths, allDeps) = List.unzip assocList;
                  let firstPartyModules =
                    List.map
                      paths f::(fun path => fileNameNoExtNoDir suffix::".re" path |> String.capitalize);
                  /* third-party deps */
                  List.concat allDeps |>
                    List.dedup |>
                    List.filter f::(fun m => not (List.exists firstPartyModules f::(fun m' => m == m')))
                }
              );
          /* Dep.all thirdPartyDepsDeps *>>| (
             fun depsDeps => */
          /* print_endline "555555555555555";
             ignore @@ taplp 0 buildRoots;
             print_endline "666666666666666666666666"; */
          /* List.iter
               depsDeps
               f::(
                 fun l => {
                   print_endline "-------";
                   List.iter l f::print_endline;
                   print_endline "--===-"
                 }
               );
             print_endline "666666666-------------66"; */
          topologicallySort
            /* TODO: don't hard-code this */
            firstNode::"Hi"
            (List.zip_exn (List.map buildRoots f::(fun r => Path.basename r |> String.capitalize)) depsDeps)
          /* ) */
        }
      )
      /* let thirdPartyDepsDeps =
           List.map
             thirdPartyBuildRoots
             f::(
               fun buildRoot => {
                 let dependenciesFilePath = rel dir::buildRoot "dependencies";
                 Dep.contents dependenciesFilePath *>>| (
                   fun raw => {
                     let assocList = parseOcamlDepModulesOutput dir::buildRoot raw;
                     let (paths, allDeps) = List.unzip assocList;
                     let firstPartyModules =
                       List.map
                         paths f::(fun path => fileNameNoExtNoDir suffix::".re" path |> String.capitalize);
                     /* third-party deps */
                     List.concat allDeps |>
                       List.dedup |>
                       List.filter f::(fun m => not (List.exists firstPartyModules f::(fun m' => m == m')))
                   }
                 )
               }
             );
         Dep.both
           (Dep.all thirdPartyDepsDeps)
           (
             Dep.all_unit (
               List.map
                 (taplp 1 thirdPartyBuildRoots)
                 f::(fun buildRoot => Dep.path (rel dir::buildRoot "dependencies"))
             )
           )
           *>>| (
           fun (depsDeps, _) => {
             print_endline "555555555555555";
             ignore @@ taplp 0 thirdPartyBuildRoots;
             print_endline "666666666666666666666666";
             List.iter
               depsDeps
               f::(
                 fun l => {
                   print_endline "-------";
                   List.iter l f::print_endline;
                   print_endline "--===-"
                 }
               );
             print_endline "666666666-------------66";
             topologicallySort
               /* TODO: don't hard-code this */
               firstNode::"Hi"
               (
                 List.zip_exn
                   (List.map thirdPartyBuildRoots f::(fun r => Path.basename r |> String.capitalize)) depsDeps
               )
           }
         ) */
    }
  )
};

sortTransitiveThirdParties;

let compileLib
    isTopLevelLib::isTopLevelLib=true
    srcDir::srcDir
    libName::libName
    buildDir::buildDir
    nodeModulesRoot::nodeModulesRoot
    buildDirRoot::buildDirRoot => {
  ignore isTopLevelLib;
  let moduleAliasFilePath = rel dir::buildDir (libName ^ ".re");
  let moduleAliasCmoPath = rel dir::buildDir (libName ^ ".cmo");
  let moduleAliasCmiPath = rel dir::buildDir (libName ^ ".cmi");
  Scheme.dep (
    Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
      fun unsortedPaths => {
        let filteredUnsortedPaths = unsortedPaths;
        sortPathsTopologically dir::srcDir paths::filteredUnsortedPaths *>>= (
          fun sortedPaths => getDepModules dir::srcDir sourcePaths::sortedPaths *>>| (
            fun (rawOutput, assocList) => {
              let rawDepsOutputRules = [
                Rule.simple
                  targets::[rel dir::buildDir "dependencies"]
                  deps::(List.map filteredUnsortedPaths f::Dep.path)
                  action::(bashf dir::buildDir "echo %s > dependencies" (Shell.escape rawOutput)),
                Rule.default dir::buildDir [Dep.path (rel dir::buildDir "dependencies")]
              ];
              /* module alias file generation */
              let moduleAliasContent =
                List.map
                  filteredUnsortedPaths
                  f::(
                    fun path => {
                      let name = fileNameNoExtNoDir path suffix::".re";
                      Printf.sprintf
                        "let module %s = %s__%s;" (String.capitalize name) (String.capitalize libName) name
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
                          dir::buildDir
                          "echo %s > %s"
                          (Shell.escape moduleAliasContent)
                          (Path.basename moduleAliasFilePath)
                    )
                  ),
                Rule.default
                  dir::buildDir
                  [Dep.path moduleAliasFilePath /* , ...List.map filteredUnsortedPaths f::Dep.path */]
              ];
              let moduleAliasCompileRules = [
                Rule.create
                  targets::[moduleAliasCmoPath, tapp 1 moduleAliasCmiPath]
                  (
                    Dep.path moduleAliasFilePath *>>| (
                      fun () =>
                        bashf
                          dir::buildDir
                          "ocamlc -pp refmt -g -no-alias-deps -w -49 -c -impl %s -o %s"
                          (Path.basename moduleAliasFilePath)
                          (Path.basename moduleAliasCmoPath)
                    )
                  ),
                Rule.default
                  dir::buildDir
                  [
                    Dep.path moduleAliasCmoPath,
                    Dep.path moduleAliasCmiPath
                    /* ...List.map filteredUnsortedPaths f::Dep.path */
                  ]
              ];
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
                      let firstPartyModules =
                        List.filter
                          (tapl 1 modules)
                          f::(
                            fun m =>
                              List.exists
                                sortedPaths
                                f::(
                                  fun path => (fileNameNoExtNoDir suffix::".re" path |> String.capitalize) == m
                                )
                          );
                      let thirdPartyModules =
                        List.filter
                          modules
                          f::(
                            fun m => not (
                              List.exists
                                sortedPaths
                                f::(
                                  fun path => (fileNameNoExtNoDir suffix::".re" path |> String.capitalize) == m
                                )
                            )
                          );
                      let firstPartyModuleDeps =
                        List.map
                          (tapl 1 firstPartyModules)
                          /* compiling here only needs cmi */
                          f::(
                            fun m => Dep.path (
                              rel dir::buildDir (libName ^ "__" ^ String.uncapitalize m ^ ".cmi")
                            )
                          );
                      let outNameNoExtNoDir = libName ^ "__" ^ fileNameNoExtNoDir path suffix::".re";
                      let outCmi = rel dir::buildDir (outNameNoExtNoDir ^ ".cmi") |> tapp 1;
                      let outCmo = rel dir::buildDir (outNameNoExtNoDir ^ ".cmo");
                      /* print_endline @@ (libName ^ "88888888888888");
                         print_endline @@ ts buildDirRoot;
                         List.iter
                           f::(
                             fun a => {
                               print_string @@ ts a;
                               print_endline "4444444444444"
                             }
                           )
                           (
                             List.map
                               thirdPartyModules f::(fun m => String.uncapitalize m |> rel dir::buildDirRoot)
                           );
                         print_endline @@ (libName ^ "8888888-----8888888"); */
                      [
                        Rule.create
                          targets::[outCmi, outCmo]
                          (
                            Dep.all_unit (
                              [
                                Dep.path path,
                                Dep.path moduleAliasCmiPath,
                                Dep.path moduleAliasCmoPath,
                                Dep.path moduleAliasFilePath
                              ] @
                                firstPartyModuleDeps @
                                /* TODO: this causes failure */
                                List.map
                                  (tapl 0 thirdPartyModules)
                                  f::(
                                    fun m => {
                                      let libName = String.uncapitalize m;
                                      rel dir::(rel dir::buildDirRoot libName) (libName ^ ".cmi") |> Dep.path
                                      /* (String.uncapitalize m) ^ "/lib.cma" |> rel dir::buildDirRoot |> Dep.path */
                                    }
                                  )
                            )
                              *>>| (
                              fun () =>
                                bashf
                                  dir::buildDir
                                  "ocamlc -pp refmt -g -open %s -I %s %s -o %s -intf-suffix rei -c -impl %s"
                                  (String.capitalize libName)
                                  (ts buildDir)
                                  (
                                    List.map
                                      thirdPartyModules
                                      f::(
                                        fun m => "-I " ^ (
                                          String.uncapitalize m |>
                                            rel dir::buildDirRoot |> Path.reach_from dir::buildDir
                                        )
                                      ) |>
                                      String.concat sep::" "
                                  )
                                  outNameNoExtNoDir
                                  (Path.reach_from dir::buildDir path)
                            )
                          ),
                        Rule.default dir::buildDir [Dep.path outCmi, Dep.path outCmo]
                      ]
                    }
                  );
              let cmos =
                List.map
                  sortedPaths
                  f::(
                    fun path => {
                      let outNameNoExtNoDir = libName ^ "__" ^ fileNameNoExtNoDir path suffix::".re";
                      rel dir::buildDir (outNameNoExtNoDir ^ ".cmo")
                    }
                  );
              let cmaPath = rel dir::buildDir "lib.cma";
              let cmaCompileRulesScheme =
                if isTopLevelLib {
                  Scheme.dep @@
                    sortTransitiveThirdParties
                      topLibName::libName nodeModulesRoot::nodeModulesRoot buildDirRoot::buildDirRoot
                      *>>| (
                      fun thirdPartyTransitiveFuckingModules => {
                        let transitiveCmaPaths =
                          List.map
                            thirdPartyTransitiveFuckingModules
                            f::(fun t => rel dir::(rel dir::buildDirRoot (String.uncapitalize t)) "lib.cma");
                        Scheme.rules [
                          Rule.simple
                            targets::[rel dir::buildDir "lib.cma"]
                            deps::(
                              [Dep.path moduleAliasCmoPath] @
                                List.map cmos f::Dep.path @ List.map transitiveCmaPaths f::Dep.path
                            )
                            action::(
                              bashf
                                dir::buildDir
                                "ocamlc -g -open %s -a -o %s %s %s %s"
                                (String.capitalize libName)
                                (Path.basename cmaPath)
                                (
                                  taplp 1 transitiveCmaPaths |>
                                    List.map f::(Path.reach_from dir::buildDir) |> String.concat sep::" "
                                )
                                (Path.basename moduleAliasCmoPath)
                                (List.map cmos f::Path.basename |> String.concat sep::" ")
                            ),
                          Rule.default dir::buildDir [Dep.path cmaPath]
                        ]
                      }
                    )
                } else {
                  Scheme.rules [
                    Rule.simple
                      targets::[rel dir::buildDir "lib.cma"]
                      deps::[Dep.path moduleAliasCmoPath, ...List.map cmos f::Dep.path]
                      action::(
                        bashf
                          dir::buildDir
                          "ocamlc -g -open %s -a -o %s %s %s"
                          (String.capitalize libName)
                          (Path.basename cmaPath)
                          (Path.basename moduleAliasCmoPath)
                          (List.map cmos f::Path.basename |> String.concat sep::" ")
                      ),
                    Rule.default dir::buildDir [Dep.path cmaPath]
                  ]
                };
              let finalOutputRules =
                if isTopLevelLib {
                  let topOutputPath = rel dir::buildDir "output.out";
                  [
                    /* ocamlc -g -o _build/hi/entry.out _build/hi/lib.cma _build/hi/hi__main.cmo */
                    Rule.simple
                      targets::[topOutputPath]
                      deps::[Dep.path cmaPath, Dep.path (rel dir::buildDir "hi__main.cmo")]
                      action::(
                        bashf
                          dir::buildDir
                          "ocamlc -g -o %s %s %s"
                          (Path.basename topOutputPath)
                          (Path.basename cmaPath)
                          "hi__main.cmo"
                      ),
                    Rule.default dir::buildDir [Dep.path topOutputPath]
                  ]
                } else {
                  []
                };
              Scheme.all [
                Scheme.rules (
                  rawDepsOutputRules @
                    moduleAliasContentRules @
                    moduleAliasCompileRules @
                    sourcesCompileRules @
                    finalOutputRules
                ),
                cmaCompileRulesScheme
              ]
            }
          )
        )
      }
    )
  )
};

let scheme dir::dir => {
  let nodeModulesRoot = Path.root_relative "node_modules";
  let buildDirRoot = Path.root_relative "_build";
  let buildDirHi = Path.root_relative "_build/hi";
  let libNameHi = "hi";
  ignore dir;
  ignore buildDirRoot;
  ignore nodeModulesRoot;
  ignore buildDirHi;
  ignore libNameHi;
  print_endline @@ (ts dir ^ "<<<<<<<<<<<<<<<");
  if (dir == root || dir == rel dir::root "src") {
    Scheme.all [
      Scheme.rules [Rule.default dir::dir [Dep.path (rel dir::buildDirHi "output.out")]]
      /* TODO: check the other TODO. Below should be removable */
      /* Scheme.dep (
           Dep.subdirs dir::nodeModulesRoot *>>| (
             fun thirdPartyDepsRoots => {
               let a = 1;
               ignore a;
               Scheme.all (
                 List.map
                   thirdPartyDepsRoots
                   f::(
                     fun depRoot => {
                       let libName = Path.basename depRoot;
                       let buildDir = rel dir::buildDirRoot libName;
                       Scheme.rules [Rule.default dir::dir [Dep.path (rel dir::buildDir "lib.cma")]]
                     }
                   )
               )
             }
           )
         ) */
    ]
  } else if (
    Path.is_descendant dir::(rel dir::root "_build") dir
  ) {
    let libName = Path.basename dir;
    let srcDir =
      if (libName == topLibName) {
        rel dir::root "src"
      } else {
        rel dir::(rel dir::nodeModulesRoot libName) "src"
      };
    compileLib
      srcDir::srcDir
      isTopLevelLib::(libName == libNameHi)
      libName::libName
      buildDir::(rel dir::buildDirRoot libName)
      buildDirRoot::buildDirRoot
      nodeModulesRoot::nodeModulesRoot
  } else {
    Scheme.no_rules
  }
};

let env = Env.create
  /* artifacts::(
       fun dir::dir => {
         print_endline @@ (ts dir ^ "00000000000000000");
         if (dir == buildDir || Path.is_descendant dir::dir buildDir) {
           Dep.glob_listing (Glob.create dir::buildDir "*.cmi")
         } else {
           Dep.return []
         }
       }
     ) */
  scheme;

let setup () => Deferred.return env;
