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
        let pathsString = List.map paths f::(fun a => " -impl " ^ Path.basename a) |> String.concat sep::" ";
        bashf dir::dir "ocamldep -pp refmt -sort -one-line %s" pathsString
      }
    )
  ) *>>| (
  fun string => String.split string on::' ' |> List.filter f::non_blank |> List.map f::(rel dir::dir)
);

sortPathsTopologically;

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
  fun string =>
    String.strip (tap 1 string) |>
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

let topLibName = "hi";

topLibName;

let compileLib
    isTopLevelLib::isTopLevelLib=true
    srcDir::srcDir
    libName::libName
    buildDir::buildDir
    buildDirRoot::buildDirRoot => {
  ignore isTopLevelLib;
  let moduleAliasFilePath = rel dir::buildDir (libName ^ ".re");
  let moduleAliasCmoPath = rel dir::buildDir (libName ^ ".cmo");
  let moduleAliasCmiPath = rel dir::buildDir (libName ^ ".cmi");
  Scheme.rules_dep (
    Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
      fun unsortedPaths => {
        /* discard third party deps, compile them separately */
        let filteredUnsortedPaths = unsortedPaths;
        sortPathsTopologically dir::srcDir paths::filteredUnsortedPaths *>>= (
          fun sortedPaths => getDepModules dir::srcDir sourcePaths::sortedPaths *>>| (
            fun assocList => {
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
                      [
                        Rule.create
                          targets::[outCmi, outCmo]
                          (
                            Dep.all_unit [
                              Dep.path path,
                              Dep.path moduleAliasCmiPath,
                              Dep.path moduleAliasCmoPath,
                              Dep.path moduleAliasFilePath,
                              ...firstPartyModuleDeps
                            ]
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
              let cmaCompileRules = [
                /* ocamlc -g -open Hi -a -o
                   _build/hi/lib.cma
                   _build/hi/hi.cmo
                   _build/hi/hi__b.cmo
                   _build/hi/hi__a.cmo
                   _build/hi/hi__c.cmo
                   _build/hi/hi__main.cmo
                    */
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
              ];
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
              moduleAliasContentRules @
                moduleAliasCompileRules @ sourcesCompileRules @ cmaCompileRules @ finalOutputRules
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
  let srcDirChenglou = Path.root_relative "node_modules/chenglou/src";
  let buildDirChenglou = Path.root_relative "_build/chenglou";
  let libNameChenglou = "chenglou";
  let srcDirHi = Path.root_relative "src";
  let buildDirHi = Path.root_relative "_build/hi";
  let libNameHi = "hi";
  ignore dir;
  ignore buildDirRoot;
  ignore nodeModulesRoot;
  ignore srcDirChenglou;
  ignore buildDirChenglou;
  ignore libNameChenglou;
  ignore srcDirHi;
  ignore buildDirHi;
  ignore libNameHi;
  print_endline @@ (ts dir ^ "<<<<<<<<<<<<<<<");
  if (dir == root || Path.basename dir == "src") {
    Scheme.all [
      /* Scheme.rules_dep (
           Dep.glob_listing (Glob.create dir::srcDirHi "*.re") *>>| (
             fun paths =>
               List.map
                 paths
                 f::(
                   fun path => {
                     ignore path;
                     let outNameNoExtNoDir = libNameHi ^ "__" ^ fileNameNoExtNoDir path suffix::".re";
                     let outCmi = rel dir::buildDirHi (outNameNoExtNoDir ^ ".cmi") |> tapp 1;
                     let outCmo = rel dir::buildDirHi (outNameNoExtNoDir ^ ".cmo");
                     Rule.default dir::dir [Dep.path (tapp 1 outCmi), Dep.path outCmo]
                   }
                 )
           )
         ), */
      Scheme.rules [Rule.default dir::dir [Dep.path (rel dir::buildDirHi "output.out")]],
      Scheme.dep (
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
                    /* let srcDir = rel dir::depRoot "src"; */
                    let buildDir = rel dir::buildDirRoot libName;
                    Scheme.rules [Rule.default dir::dir [Dep.path (rel dir::buildDir "lib.cma")]]
                    /* Scheme.rules_dep (
                         Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>| (
                           fun paths =>
                             List.map
                               paths
                               f::(
                                 fun path => {
                                   ignore path;
                                   let outNameNoExtNoDir =
                                     libName ^ "__" ^ fileNameNoExtNoDir path suffix::".re";
                                   let outCmi = rel dir::buildDir (outNameNoExtNoDir ^ ".cmi") |> tapp 1;
                                   let outCmo = rel dir::buildDir (outNameNoExtNoDir ^ ".cmo");
                                   Rule.default dir::dir [Dep.path (tapp 1 outCmi), Dep.path outCmo]
                                 }
                               )
                         )
                       ) */
                  }
                )
            )
          }
        )
      )
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
    Scheme.all [
      compileLib
        srcDir::srcDir
        isTopLevelLib::true
        libName::libName
        buildDir::(rel dir::buildDirRoot libName)
        buildDirRoot::buildDirRoot
    ]
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
