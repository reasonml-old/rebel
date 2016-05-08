open Core.Std;

open Async.Std;

open Jenga_lib.Api;

/* let ( *>>| ) = Dep.map; */
let bash dir::dir command => Action.process dir::dir prog::"bash" args::["-c", command] ();

let bashf dir::dir fmt => ksprintf (fun str => bash dir::dir str) fmt;

let buildDir = Path.root_relative "_build";

let srcDir = Path.root_relative "src";

let scheme dir::dir => {
  ignore dir;
  /* if (dir == buildDir || dir == Path.the_root) { */
  let inin = Path.relative dir::srcDir "a.re";
  let a =
    Rule.simple
      targets::[Path.relative dir::srcDir "a.out"]
      deps::[Dep.path inin]
      action::(bashf dir::srcDir "cat %s > %s" "a.re" "a.out");
  Scheme.rules [a, a, Rule.default dir::dir [Dep.path @@ Path.relative dir::srcDir "a.out"]]
  /* } else {
       Scheme.no_rules
     } */
};

let env =
  Env.create
    artifacts::(
      fun dir::dir => {
        print_endline (Path.to_string dir ^ ",,,,,,,,,,,,,,,");
        Dep.buildable_targets dir::buildDir
        /* Dep.return [] */
        /* if (dir == srcDir) {
             Dep.source_files dir::dir
           } else {
             Dep.source_files dir::dir
           } */
      }
    )
    scheme;

let setup () => Deferred.return env;
