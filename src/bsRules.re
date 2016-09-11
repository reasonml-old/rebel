open Core.Std;

open Jenga_lib.Api;

let jsOutput =
  Path.relative dir::(Path.relative dir::Utils.buildDirRoot (Utils.tsl Utils.topLibName)) "app.js";

let bsScheme dir::dir =>
  if (dir == Path.the_root) {
    Scheme.all [
      Scheme.rules [ Rule.default dir::dir [Dep.path jsOutput]]
    ]
  } else {
    Scheme.no_rules
  };
