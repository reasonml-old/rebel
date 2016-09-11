open Core.Std;

open Jenga_lib.Api;

let jsOutput =
  Path.relative
    dir::(Path.relative dir::Utils.buildDirRoot (Utils.tsl Utils.topLibName)) "Index.js";

let compileSourcesScheme
    libDir::libDir
    buildDir::buildDir
    libName::libName
    sourcePaths::sourcePaths => {
  let sourcePath = List.nth_exn sourcePaths 0;
  let fileName ext::ext => Path.relative dir::buildDir (Utils.fileNameNoExtNoDir sourcePath ^ ext);
  let jsp = fileName ".js";
  let cmi = fileName ".cmi";
  let cmj = fileName ".cmj";
  let cmt = fileName ".cmt";
  let action =
    Utils.bashf
      /*
          More Flags:
         -bs-package-output  set npm-output-path: [opt_module]:path, for example: 'lib/cjs', 'amdjs:lib/amdjs' and 'goog:lib/gjs'

         -bs-package-name is set `self` to that it produces right require calls.
       */
      "bsc -g -pp refmt -bin-annot -bs-package-name self -bs-package-output commonjs:%s -I %s -o %s -c -impl %s"
      (Utils.tsp buildDir)
      (Utils.tsp buildDir)
      (Utils.tsp jsp)
      (Utils.tsp sourcePath);
  let targets = [cmi, cmj, cmt, jsp];
  /* let deps = []; */
  Scheme.rules [Rule.create targets::targets (Dep.return action)]
};

let compileLibScheme
    libName::libName
    isTopLevelLib::isTopLevelLib
    srcDir::srcDir
    buildDir::buildDir => {
  /* let bindD' f dep => Dep.bind dep f; */
  let mapD' f dep => Dep.map dep f;
  Dep.glob_listing (Glob.create dir::srcDir "*.{re,rei,ml,mli}") |>
  mapD' (
    fun unsortedPaths => {
      List.iter unsortedPaths f::(fun x => print_endline (Utils.tsp x));
      compileSourcesScheme
        libDir::(Path.dirname srcDir)
        buildDir::buildDir
        libName::libName
        sourcePaths::unsortedPaths
    }
  ) |> Scheme.dep
};

let bsScheme dir::dir =>
  if (dir == Path.the_root) {
    Scheme.all [Scheme.rules [Rule.default dir::dir [Dep.path jsOutput]]]
  } else if (
    Path.is_descendant dir::Utils.buildDirRoot dir
  ) {
    let dirName = Path.basename dir;
    let libName = Utils.Lib (Path.basename dir);
    let isTopLevelLib = libName == Utils.topLibName;
    let srcDir =
      isTopLevelLib ?
        Utils.topSrcDir :
        Path.relative dir::(Path.relative dir::Utils.nodeModulesRoot dirName) "src";
    compileLibScheme
      srcDir::srcDir
      isTopLevelLib::isTopLevelLib
      libName::libName
      buildDir::(Path.relative dir::Utils.buildDirRoot dirName)
  } else {
    Scheme.no_rules
  };
