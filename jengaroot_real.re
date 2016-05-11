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

let stdlibModules = ["List", "String", "Set", "Queue", "Printf", "Stack"];

let fileNameNoExtNoDir path suffix::suffix => Path.basename path |> String.chop_suffix_exn suffix::suffix;

fileNameNoExtNoDir;

let sortPathsTopologically dir::dir paths::paths =>
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
          List.map (taplp 1 paths) f::(fun a => " -impl " ^ Path.basename a) |> String.concat sep::" ";
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
          | [path, deps] => (
              rel dir::dir path,
              String.split deps on::' ' |>
                List.filter f::non_blank |>
                List.filter f::(fun d => not (List.exists stdlibModules f::(fun m => m == d)))
            )
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

let topLibName = "top";

topLibName;

let topologicallySort mainNode::mainNode muhGraph => {
  let rec topologicallySort' currNode muhGraph accum => {
    let nodeDeps =
      switch (List.find muhGraph f::(fun (n, _) => n == currNode)) {
      | None => raise (Invalid_argument currNode)
      | Some (_, nodeDeps) => nodeDeps
      };
    List.iter nodeDeps f::(fun dep => topologicallySort' dep muhGraph accum);
    if (not @@ List.exists accum.contents f::(fun n => n == currNode)) {
      accum := [currNode, ...accum.contents]
    }
  };
  let accum = {contents: []};
  topologicallySort' mainNode muhGraph accum;
  List.rev accum.contents |> List.filter f::(fun m => m != mainNode)
};

topologicallySort;

let sortTransitiveThirdParties
    topLibName::topLibName
    nodeModulesRoot::nodeModulesRoot
    buildDirRoot::buildDirRoot => {
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  /* this is bad bc node_modules dir might be stale (have extra modules), and jenga doesn't clean stale dirs
     correctly */
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
                  /* third-party deps of current module */
                  List.concat allDeps |>
                    List.dedup |>
                    List.filter f::(fun m => not (List.exists firstPartyModules f::(fun m' => m == m')))
                }
              );
          topologicallySort
            mainNode::(String.capitalize topLibName)
            (List.zip_exn (List.map buildRoots f::(fun r => Path.basename r |> String.capitalize)) depsDeps)
        }
      )
    }
  )
};

sortTransitiveThirdParties;

let compileLibScheme
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
  let moduleAliasCmtPath = rel dir::buildDir (libName ^ ".cmt");
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
                  action::(bashf dir::buildDir "echo %s > dependencies" (Shell.escape rawOutput))
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
                    Dep.return (
                      bashf
                        dir::buildDir
                        "echo %s > %s"
                        (Shell.escape moduleAliasContent)
                        (Path.basename moduleAliasFilePath)
                    )
                  )
              ];
              let moduleAliasCompileRules = [
                Rule.create
                  targets::[moduleAliasCmoPath, tapp 1 moduleAliasCmiPath, moduleAliasCmtPath]
                  (
                    Dep.path moduleAliasFilePath *>>| (
                      fun () =>
                        bashf
                          dir::buildDir
                          "ocamlc -pp refmt -bin-annot -g -no-alias-deps -w -49 -c -impl %s -o %s"
                          (Path.basename moduleAliasFilePath)
                          (Path.basename moduleAliasCmoPath)
                    )
                  )
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
                      let outCmt = rel dir::buildDir (outNameNoExtNoDir ^ ".cmt");
                      [
                        Rule.create
                          targets::[outCmi, outCmo, outCmt]
                          (
                            Dep.all_unit (
                              [
                                Dep.path path,
                                Dep.path moduleAliasCmiPath,
                                Dep.path moduleAliasCmoPath,
                                Dep.path moduleAliasCmtPath,
                                Dep.path moduleAliasFilePath
                              ] @
                                firstPartyModuleDeps @
                                [
                                  Dep.all_unit (
                                    List.map
                                      (tapl 1 thirdPartyModules)
                                      f::(
                                        fun m => {
                                          let libName = String.uncapitalize m;
                                          Dep.glob_listing (
                                            Glob.create
                                              dir::(rel dir::(rel dir::nodeModulesRoot libName) "src") "*.re"
                                          )
                                            *>>= (
                                            fun sources => Dep.all_unit (
                                              List.map
                                                sources
                                                f::(
                                                  fun sourcePath => Dep.path (
                                                    rel
                                                      dir::(rel dir::buildDirRoot libName)
                                                      (
                                                        libName ^
                                                          "__" ^
                                                          fileNameNoExtNoDir suffix::".re" sourcePath ^
                                                          ".cmi"
                                                      )
                                                  )
                                                )
                                            )
                                          )
                                        }
                                      )
                                  )
                                ]
                            )
                              *>>| (
                              fun () =>
                                bashf
                                  dir::buildDir
                                  "ocamlc -pp refmt -bin-annot -g -open %s -I %s %s -o %s -intf-suffix rei -c -impl %s"
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
                          )
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
                              /* TODO: what about cmi and cmt? */
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
                            )
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
                      )
                  ]
                };
              let finalOutputRules =
                if isTopLevelLib {
                  let topOutputPath = rel dir::buildDir "output.out";
                  [
                    /* ocamlc -g -o _build/hi/entry.out _build/hi/lib.cma _build/hi/hi__main.cmo */
                    Rule.simple
                      targets::[topOutputPath]
                      deps::[Dep.path cmaPath, Dep.path (rel dir::buildDir (topLibName ^ "__main.cmo"))]
                      action::(
                        bashf
                          dir::buildDir
                          "ocamlc -g -o %s %s %s"
                          (Path.basename topOutputPath)
                          (Path.basename cmaPath)
                          (topLibName ^ "__main.cmo")
                      )
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

let generateDotMerlinScheme
    nodeModulesRoot::nodeModulesRoot
    buildDirRoot::buildDirRoot
    isTopLevelLib::isTopLevelLib
    libName::libName
    dir::dir
    root::root => {
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  ignore isTopLevelLib;
  ignore libName;
  ignore dir;
  ignore root;
  let dotMerlinContent =
    Printf.sprintf
      {|%s
S %s

B %s

FLG -w -30 -w -40 -open %s
|}
      (isTopLevelLib ? "src/*" : "")
      (Path.reach_from dir::dir (rel dir::nodeModulesRoot "**/src"))
      (Path.reach_from dir::dir (rel dir::buildDirRoot "**"))
      (String.capitalize libName);
  Scheme.rules [
    Rule.simple
      targets::[rel dir::dir ".merlin"]
      deps::[]
      action::(bashf dir::dir "echo %s > .merlin" (Shell.escape dotMerlinContent))
  ]
};

let scheme dir::dir => {
  let nodeModulesRoot = Path.root_relative "node_modules";
  let buildDirRoot = Path.root_relative "_build";
  ignore dir;
  ignore buildDirRoot;
  ignore nodeModulesRoot;
  /* print_endline @@ (ts dir ^ "<<<<<<<<<<<<<<<"); */
  /* all the third party .merlin files, generated into their own node_modules/bla dir. We're making an
     exception here to generate artifact into somewhere else than _build/. Maybe one day merlin will
     support our dir structure */
  if (dir == root) {
    let dotMerlinScheme = Scheme.rules_dep (
      Dep.subdirs dir::nodeModulesRoot *>>| (
        fun thirdPartyNodeModulesRoots =>
          List.map
            thirdPartyNodeModulesRoots
            f::(fun path => Rule.default dir::dir [Dep.path (rel dir::path ".merlin")])
      )
    );
    let dotMerlinGenScheme =
      generateDotMerlinScheme
        buildDirRoot::buildDirRoot
        isTopLevelLib::true
        nodeModulesRoot::nodeModulesRoot
        dir::dir
        root::root
        libName::topLibName;
    Scheme.all [
      dotMerlinGenScheme,
      Scheme.rules [
        Rule.default
          dir::dir
          [
            Dep.path (rel dir::(rel dir::buildDirRoot topLibName) "output.out"),
            Dep.path (rel dir::root ".merlin")
          ]
      ],
      dotMerlinScheme
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
    compileLibScheme
      srcDir::srcDir
      isTopLevelLib::(libName == topLibName)
      libName::libName
      buildDir::(rel dir::buildDirRoot libName)
      buildDirRoot::buildDirRoot
      nodeModulesRoot::nodeModulesRoot
  } else if (
    Path.dirname dir == nodeModulesRoot
  ) {
    let libName = Path.basename dir;
    generateDotMerlinScheme
      buildDirRoot::buildDirRoot
      isTopLevelLib::false
      nodeModulesRoot::nodeModulesRoot
      dir::dir
      root::root
      libName::libName
  } else {
    Scheme.no_rules
  }
};

let env = Env.create
  /* TODO: this doesn't traverse down to _build so I can't ask it to clean files there? */
  /* artifacts::(
       fun dir::dir => {
         print_endline @@ (ts dir ^ "00000000000000000");
         /* if (dir == buildDir || Path.is_descendant dir::dir buildDir) {
           Dep.glob_listing (Glob.create dir::buildDir "*.cmi")
         } else { */
           Dep.return []
         /* } */
       }
     ) */
  scheme;

let setup () => Deferred.return env;
