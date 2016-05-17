/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Core.Std;

open Async.Std;

open Jenga_lib.Api;

/* general helpers */
let ( *>>| ) = Dep.map;

let ( *>>= ) = Dep.bind;

let rel = Path.relative;

let ts = Path.to_string;

let root = Path.the_root;

let bash dir::dir command => Action.process dir::dir prog::"bash" args::["-c", command] ();

let bashf dir::dir fmt => ksprintf (fun str => bash dir::dir str) fmt;

let nonBlank s =>
  switch (String.strip s) {
  | "" => false
  | _ => true
  };

/* assumes there is a suffix to chop. Throws otherwise */
let chopSuffixExn str => String.slice str 0 (String.rindex_exn str '.');

let fileNameNoExtNoDir path => Path.basename path |> chopSuffixExn;

fileNameNoExtNoDir;

/* generic testing/logging helpers */
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

let tapAssocList n a =>
  if (n == 0) {
    print_endline "tapAssocList---------";
    List.iter
      a f::(fun (path, deps) => print_endline @@ (path ^ ": " ^ String.concat sep::" " deps));
    a
  } else {
    a
  };

tapAssocList;

/* this jengaroot-specific helpers */
let topLibName = "top";

let finalOutputName = "app";

let libraryFileName = "lib.cma";

let nodeModulesRoot = rel dir::root "node_modules";

let buildDirRoot = rel dir::root "_build";

let topSrcDir = rel dir::root "src";

/* revert these to String.capitalize and String.uncapitalize to recover *from* our new assumption that file
   names are capitalized */
let stringCapitalize a => a;

let stringUncapitalize a => a;

stringCapitalize;

stringUncapitalize;

/* let allThirdPartyDeps = Dep.subdirs dir::nodeModulesRoot *>>| (
     fun paths =>
       List.map paths f::(fun path => fileNameNoExtNoDir  path |> String.uppercase)
   );

   allThirdPartyDeps; */
let ocamlDepModules sourcePath::sourcePath => Dep.subdirs dir::nodeModulesRoot *>>= (
  fun subdirs => {
    let execDir = Path.dirname sourcePath;
    let thirdPartyBuildRoots =
      List.map subdirs f::(fun subdir => rel dir::buildDirRoot (Path.basename subdir));
    Dep.action_stdout (
      Dep.all_unit [
        Dep.path sourcePath,
        Dep.all_unit (
          List.map
            thirdPartyBuildRoots
            f::(fun buildRoot => rel dir::buildRoot (Path.basename buildRoot ^ ".cmi") |> Dep.path)
        )
      ]
        *>>| (
        fun () =>
          bashf
            dir::execDir
            "ocamldep -pp refmt %s -ml-synonym .re -mli-synonym .rei -one-line %s"
            (
              thirdPartyBuildRoots |>
                List.map f::(fun path => Path.reach_from dir::execDir path) |>
                List.map f::(fun path => "-I " ^ path) |>
                String.concat sep::" "
            )
            (Path.basename sourcePath)
      )
    )
      *>>| (
      fun string =>
        switch (
          String.strip string |> String.split on::'\n' |> List.hd_exn |> String.split on::':'
        ) {
        | [original, deps] =>
          String.split deps on::' ' |>
            List.filter f::nonBlank |>
            List.map f::chopSuffixExn |>
            /* TODO: mother of all fragile logic. */
            /* node_modules/bookshop/src/Index.cmo : node_modules/bookshop/src/Index.cmi */
            /* ^ this means that there's a rei interface file for it. Filter it out */
            List.filter f::(fun m => m != chopSuffixExn original) |>
            /* TODO: fragile logic. The result might be ../_build/bookshop/bookshop.cmo so this is how we
               temporarily detect it */
            List.map
              f::(
                fun m =>
                  switch (String.rindex m '/') {
                  | None => m
                  | Some idx => String.slice m (idx + 1) (String.length m) |> String.capitalize
                  }
              ) |>
            tapl 1
        | _ => failwith "expected exactly one ':' in ocamldep output line"
        }
    )
  }
);

ocamlDepModules;

let getThirdPartyDepsForLib srcDir::srcDir => Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
  fun sourcePaths =>
    Dep.all (List.map sourcePaths f::(fun sourcePath => ocamlDepModules sourcePath::sourcePath)) *>>| (
    fun sourcePathsDeps => {
      let internalDeps =
        List.map sourcePaths f::(fun path => fileNameNoExtNoDir path |> stringCapitalize);
      List.concat sourcePathsDeps |>
        List.dedup |>
        List.filter f::(fun dep => not (List.exists internalDeps f::(fun dep' => dep == dep')))
    }
  )
);

getThirdPartyDepsForLib;

/* this isn't completely generic. mainNode is the start node. We potentially don't traverse the whole graph,
   only the subgraph connected to mainNode (aka dangling dependencies not used by mainNode are ignored) */
let topologicalSort mainNode::mainNode assocListGraph => {
  ignore @@ tapAssocList 1 assocListGraph;
  let rec topologicalSort' currNode assocListGraph accum => {
    let nodeDeps =
      switch (List.find assocListGraph f::(fun (n, _) => n == currNode)) {
      /* node not found: presume to be third-party dep. This is slightly dangerous because it might also mean
         we didn't construct the graph correctly. */
      | None => []
      | Some (_, nodeDeps') => nodeDeps'
      };
    List.iter nodeDeps f::(fun dep => topologicalSort' dep assocListGraph accum);
    if (not @@ List.exists accum.contents f::(fun n => n == currNode)) {
      accum := [currNode, ...accum.contents]
    }
  };
  let accum = {contents: []};
  topologicalSort' mainNode assocListGraph accum;
  List.rev accum.contents
};

topologicalSort;

let sortTransitiveThirdParties topLibName::topLibName => {
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  getThirdPartyDepsForLib srcDir::topSrcDir *>>= (
    fun topThirdPartyDeps => {
      let thirdPartiesSrcDirs =
        List.map
          topThirdPartyDeps
          f::(fun dep => rel dir::(rel dir::nodeModulesRoot (String.uncapitalize dep)) "src");
      let thirdPartiesThirdPartyDepsD = Dep.all (
        List.map thirdPartiesSrcDirs f::(fun srcDir => getThirdPartyDepsForLib srcDir::srcDir)
      );
      let topLibModuleName = String.capitalize topLibName;
      thirdPartiesThirdPartyDepsD *>>| (
        fun thirdPartiesThirdPartyDeps =>
          List.zip_exn
            [topLibModuleName, ...topThirdPartyDeps]
            [topThirdPartyDeps, ...thirdPartiesThirdPartyDeps] |>
            topologicalSort mainNode::topLibModuleName |>
            /* remove the top lib node itself from the sorted result */
            List.filter f::(fun m => m != topLibModuleName)
      )
    }
  )
};

let sortPathsTopologically buildDirRoot::buildDirRoot libName::libName dir::dir paths::paths => {
  ignore buildDirRoot;
  ignore libName;
  /* don't remove this piece of code! This is the `ocamldep -sort`-less version. Except it doesn't traverse
     the whole graph (see comment on topologicalSort) so we get free dead code elimination. But we do want
     merlin to pick up newly added files, and DCE means they're not even compiled. We'll change
     topologicalSort to traverse the whole graph soon. */
  /* let pathsAsModules =
       List.map paths f::(fun path => fileNameNoExtNoDir  path |> stringCapitalize);
     let sourcePathsDepsD = Dep.all (List.map paths f::(fun path => ocamlDepModules sourcePath::path));
     sourcePathsDepsD *>>| (
       fun sourcePathsDeps =>
         List.zip_exn pathsAsModules sourcePathsDeps |>
           topologicalSort mainNode::"Main" |>
           List.filter f::(fun m => List.exists pathsAsModules f::(fun m' => m' == m)) |>
           List.map f::(fun m => rel dir::dir (stringUncapitalize m ^ ".re")) |>
           taplp 0
     ) */
  Dep.action_stdout (
    Dep.all_unit (List.map paths f::Dep.path) *>>| (
      fun () => {
        let pathsString = List.map (taplp 1 paths) f::Path.basename |> String.concat sep::" ";
        bashf
          dir::dir
          "ocamldep -pp refmt -ml-synonym .re -mli-synonym .rei -sort -one-line %s"
          (tap 1 pathsString)
      }
    )
  )
    *>>| (
    fun string =>
      String.split string on::' ' |>
        List.filter f::nonBlank |> List.map f::(rel dir::dir) |> taplp 1
  )
};

sortPathsTopologically;

/* the module alias file takes the current library foo's first-party sources, e.g. A.re, B.re, and turn them
   into a foo.re file whose content is:
   let module A = Foo__A;
   let module B = Foo__B;
   */
/* We'll then compile this file into foo.cmi/cmo/cmt, and have it opened by default when compiling A.re and
   B.re (into foo_A and foo_B respectively) later. The effect is that, inside A.re, we can refer to B instead
   of Foo__B thanks to the pre-opened foo.re. But when these files are used by other libraries (which aren't
   compiled with foo.re pre-opened of course), they won't see module A or B, only Foo__A and Foo__B, aka in
   practice, they simply won't see them. This effectively means we've implemented namespacing! */
let moduleAliasFileScheme buildDir::buildDir sourceModules::sourceModules libName::libName => {
  let name extension => rel dir::buildDir (libName ^ "." ^ extension);
  let sourcePath = name "re";
  let cmo = name "cmo";
  let cmi = name "cmi";
  let cmt = name "cmt";
  let fileContent =
    List.map
      sourceModules
      f::(
        fun moduleName =>
          Printf.sprintf
            "let module %s = %s__%s;\n" moduleName (String.capitalize libName) moduleName
      ) |>
      String.concat sep::"";
  let contentRule =
    Rule.create targets::[sourcePath] (Dep.return (Action.save fileContent target::sourcePath));
  let compileRule =
    Rule.create
      targets::[cmo, cmi, cmt]
      (
        Dep.path sourcePath *>>| (
          fun () =>
            bashf
              dir::buildDir
              /* We suppress a few warnings here through -w.
                 - 49: Absent cmi file when looking up module alias. Aka Foo__A and Foo__B's compiled cmis
                 can't be found at the moment this module alias file is compiled. This is normal, since the
                 module alias file is the first thing that's compiled (so that we can open it during
                 compilation of A.re and B.re into Foo__A and Foo__B). Think of this as forward declaration.

                 - 30: Two labels or constructors of the same name are defined in two mutually recursive
                 types. I forgot...

                 - 40: Constructor or label name used out of scope. I forgot too. Great comment huh?

                 More flags:
                 -pp refmt option makes ocamlc take our reason syntax source code and pass it through our
                 refmt parser first, before operating on the AST.

                 -bin-annot: generates cmt files that contains info such as types and bindings, for use with
                 Merlin.

                 -g: add debugging info. You don't really ever compile without this flag.

                 -impl: source file. This flag's needed if the source extension isn't ml. I think.

                 -o: output name
                 */
              "ocamlc -pp refmt -bin-annot -g -no-alias-deps -w -49 -w -30 -w -40 -c -impl %s -o %s"
              (Path.basename sourcePath)
              (Path.basename cmo)
        )
      );
  Scheme.rules [contentRule, compileRule]
};

let jsooLocationD = Dep.action_stdout (Dep.return (bash dir::root "ocamlfind query js_of_ocaml")) *>>| String.strip;

/* Cma is a library file for the current library which bundles up all the lib's first-party compiled sources.
   This way, it's much easier, at the end, at the top level, to include each library's cma file to compile the
   final executable, than to tediously pass every single source file from every lib in order.

   There's a caveat though. We said we're only bundling up the current lib's first-party code; We _could_ have
   bundled up its third-party deps' cma too, but then we might get into duplicate artifact problem caused by
   e.g. library A and B both requiring and bundling C. So we can only bundle first-party code, and then, at
   the top, take all the transitive dependencies (libraries) cmas, figure out their relative order, and pass
   them in that order to the ocamlc command. Still tedious, but at least we're not passing individual source
   files in order. */
let compileCmaScheme
    sortedSourcePaths::sortedSourcePaths
    libName::libName
    isTopLevelLib::isTopLevelLib
    buildDir::buildDir => {
  let cmaPath = rel dir::buildDir libraryFileName;
  let moduleAliasCmoPath = rel dir::buildDir (libName ^ ".cmo");
  /* For compilation */
  let cmos =
    List.map
      /* To compile one cma file, we need to pass the compiled first-party sources in order to ocamlc */
      sortedSourcePaths
      f::(fun path => rel dir::buildDir (libName ^ "__" ^ fileNameNoExtNoDir path ^ ".cmo"));
  let cmosString = List.map cmos f::Path.basename |> String.concat sep::" ";
  /* Final bundling. Time to get all the transitive dependencies... */
  if isTopLevelLib {
    Scheme.dep (
      Dep.both jsooLocationD (sortTransitiveThirdParties topLibName::libName) *>>| (
        fun (jsooLocation, thirdPartyTransitiveDeps) => {
          let transitiveCmaPaths =
            List.map
              thirdPartyTransitiveDeps
              f::(
                fun dep =>
                  rel dir::(rel dir::buildDirRoot (String.uncapitalize dep)) libraryFileName
              );
          Scheme.rules [
            Rule.simple
              targets::[cmaPath]
              deps::(
                /* TODO: I don't think cmis and cmts are being read here, so we don't need to include them. */
                [moduleAliasCmoPath] @ cmos @ transitiveCmaPaths |>
                  List.map f::Dep.path
              )
              action::(
                bashf
                  dir::buildDir
                  /* For ease of coding, we'll blindly include js_of_ocaml in the -I search path here, in case
                     the module invokes some jsoo's Js module-related stuff. */
                  /* Example command: ocamlc -g -I path/to/js_of_ocaml path/to/js_of_ocaml/js_of_ocaml.cma \
                     -open Top -a -o lib.cma  ../barDependsOnMe/lib.cma ../bar/lib.cma ../baz/lib.cma \
                     top.cmo aDependsOnMe.cmo a.cmo moreFirstPartyCmo.cmo */
                  /* Flags:
                     -I: search path(s), when ocamlc looks for modules referenced inside the file.

                     -open: compile the file as if [file being opened] was opened at the top of the file. In
                     our case, we open our module alias file generated with `moduleAliasFileScheme`. See that
                     function for more comment.

                     -a: flag for building a library.

                     -o: output file name.
                     */
                  "ocamlc -g -I %s %s/js_of_ocaml.cma -open %s -a -o %s %s %s %s"
                  jsooLocation
                  jsooLocation
                  (String.capitalize libName)
                  (Path.basename cmaPath)
                  (
                    transitiveCmaPaths |>
                      List.map f::(Path.reach_from dir::buildDir) |> String.concat sep::" "
                  )
                  (Path.basename moduleAliasCmoPath)
                  cmosString
              )
          ]
        }
      )
    )
  } else {
    Scheme.rules [
      Rule.simple
        targets::[cmaPath]
        deps::(List.map [moduleAliasCmoPath, ...cmos] f::Dep.path)
        action::(
          bashf
            dir::buildDir
            /* Example command: ocamlc -g -open Foo -a -o lib.cma foo.cmo aDependsOnMe.cmo a.cmo b.cmo */
            "ocamlc -g -open %s -a -o %s %s %s"
            (String.capitalize libName)
            (Path.basename cmaPath)
            (Path.basename moduleAliasCmoPath)
            cmosString
        )
    ]
  }
};

/* This function assumes we're using it only at the top level. */
/* We'll output an executable, plus a js_of_ocaml JavaScript file. Throughout the compilation of the source
   files, we've already mingled in the correctly jsoo search paths in ocamlc to make this final compilation
   work.

   The output step is simple because the lib.cma library file already has all the information (including
   dependencies) inside. */
let finalOutputsScheme topBuildDir::topBuildDir => {
  let cmaPath = rel dir::topBuildDir libraryFileName;
  let binaryPath = rel dir::topBuildDir (finalOutputName ^ ".out");
  let jsooPath = rel dir::topBuildDir (finalOutputName ^ ".js");
  Scheme.rules [
    Rule.simple
      targets::[binaryPath]
      /* Currently assume the entry file is Index.re, compiled into myLibraryName__Index.cmo */
      deps::[Dep.path cmaPath, Dep.path (rel dir::topBuildDir (topLibName ^ "__Index.cmo"))]
      action::(
        bashf
          dir::topBuildDir
          "ocamlc -g -o %s %s %s"
          (Path.basename binaryPath)
          (Path.basename cmaPath)
          (topLibName ^ "__Index.cmo")
      ),
    Rule.simple
      targets::[jsooPath]
      deps::[Dep.path binaryPath]
      action::(
        bashf
          dir::topBuildDir
          /* I don't know what the --linkall flag does, and does the --pretty flag work? Because the output is
             still butt ugly. Just kidding I love you guys. */
          "js_of_ocaml --source-map --no-inline --debug-info --pretty --linkall %s"
          (Path.basename binaryPath)
      )
  ]
};

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
        ignore 1;
        sortPathsTopologically
          buildDirRoot::buildDirRoot libName::libName dir::srcDir paths::unsortedPaths
          *>>| (
          fun sortedPaths => {
            let sourcesCompileRules =
              Scheme.rules_dep @@
                Dep.all @@
                List.map
                  unsortedPaths
                  f::(
                    fun path => ocamlDepModules sourcePath::path *>>| (
                      fun modules => {
                        let firstPartyModules =
                          List.filter
                            (tapl 1 modules)
                            f::(
                              fun m =>
                                List.exists
                                  unsortedPaths
                                  f::(
                                    fun path => (fileNameNoExtNoDir path |> stringCapitalize) == m
                                  )
                            );
                        let thirdPartyModules =
                          List.filter
                            modules
                            f::(
                              fun m => not (
                                List.exists
                                  unsortedPaths
                                  f::(
                                    fun path => (fileNameNoExtNoDir path |> stringCapitalize) == m
                                  )
                              )
                            );
                        let firstPartyModuleDeps =
                          List.map
                            (tapl 1 firstPartyModules)
                            /* compiling here only needs cmi */
                            f::(
                              fun m => Dep.path (
                                rel dir::buildDir (libName ^ "__" ^ stringUncapitalize m ^ ".cmi")
                              )
                            );
                        let outNameNoExtNoDir = libName ^ "__" ^ fileNameNoExtNoDir path;
                        let outCmi = rel dir::buildDir (outNameNoExtNoDir ^ ".cmi") |> tapp 1;
                        let outCmo = rel dir::buildDir (outNameNoExtNoDir ^ ".cmo");
                        let outCmt = rel dir::buildDir (outNameNoExtNoDir ^ ".cmt");
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
                                              dir::(
                                                rel dir::(rel dir::nodeModulesRoot libName) "src"
                                              )
                                              "*.re"
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
                                                          fileNameNoExtNoDir sourcePath ^
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
                                  /* TODO: explain the warning suppression */
                                  "ocamlc -pp refmt -bin-annot -g -w -30 -w -40 -open %s -I `ocamlfind query js_of_ocaml` `ocamlfind query js_of_ocaml`/js_of_ocaml.cma -I %s %s -o %s -intf-suffix rei -c -impl %s"
                                  (String.capitalize libName)
                                  /* "-I /Users/chenglou/.opam/4.02.3/lib/js_of_ocaml /Users/chenglou/.opam/4.02.3/lib/js_of_ocaml/js_of_ocaml.cma" */
                                  (ts buildDir)
                                  (
                                    List.map
                                      thirdPartyModules
                                      f::(
                                        fun m => "-I " ^ (
                                          stringUncapitalize m |>
                                            rel dir::buildDirRoot |> Path.reach_from dir::buildDir
                                        )
                                      ) |>
                                      String.concat sep::" "
                                  )
                                  outNameNoExtNoDir
                                  (Path.reach_from dir::buildDir path)
                            )
                          )
                      }
                    )
                  );
            let finalOutputsScheme =
              isTopLevelLib ? finalOutputsScheme topBuildDir::buildDir : Scheme.no_rules;
            Scheme.all [
              moduleAliasFileScheme
                buildDir::buildDir
                libName::libName
                sourceModules::(List.map unsortedPaths f::fileNameNoExtNoDir),
              sourcesCompileRules,
              compileCmaScheme
                buildDir::buildDir
                isTopLevelLib::isTopLevelLib
                libName::libName
                sortedSourcePaths::sortedPaths,
              finalOutputsScheme
            ]
          }
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

PKG js_of_ocaml

FLG -w -30 -w -40 -open %s
|}
      (isTopLevelLib ? "S src" : "")
      (Path.reach_from dir::dir (rel dir::nodeModulesRoot "**/src"))
      (Path.reach_from dir::dir (rel dir::buildDirRoot "*"))
      (String.capitalize libName);
  Scheme.rules [
    Rule.simple
      targets::[rel dir::dir ".merlin"]
      deps::[]
      action::(Action.save dotMerlinContent target::(rel dir::dir ".merlin"))
  ]
};

let scheme dir::dir => {
  ignore dir;
  /* print_endline @@ (ts dir ^ "<<<<<<<<<<<<<<<"); */
  /* all the third party .merlin files, generated into their own node_modules/bla dir. We're making an
     exception here to generate artifact into somewhere else than _build/. Maybe one day merlin will
     support our dir structure */
  if (dir == root) {
    let dotMerlinScheme = Scheme.rules_dep (
      getThirdPartyDepsForLib srcDir::topSrcDir *>>| (
        fun deps => {
          let thirdPartyNodeModulesRoots =
            List.map deps f::(fun dep => rel dir::nodeModulesRoot (String.uncapitalize dep));
          List.map
            thirdPartyNodeModulesRoots
            f::(fun path => Rule.default dir::dir [Dep.path (rel dir::path ".merlin")])
        }
      )
    );
    Scheme.all [
      generateDotMerlinScheme
        buildDirRoot::buildDirRoot
        isTopLevelLib::true
        nodeModulesRoot::nodeModulesRoot
        dir::dir
        root::root
        libName::topLibName,
      Scheme.rules [
        Rule.default
          dir::dir
          [
            Dep.path (rel dir::(rel dir::buildDirRoot topLibName) (finalOutputName ^ ".out")),
            Dep.path (rel dir::(rel dir::buildDirRoot topLibName) (finalOutputName ^ ".js")),
            Dep.path (rel dir::root ".merlin")
          ]
      ],
      dotMerlinScheme
    ]
  } else if (
    Path.is_descendant dir::buildDirRoot dir
  ) {
    let libName = Path.basename dir;
    let srcDir =
      if (libName == topLibName) {
        topSrcDir
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
