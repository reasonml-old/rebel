/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

open Jenga_lib.Api;

open Utils;

let scheme dir::dir => {
  ignore dir;
  if (backend == "bucklescript") {
    Bucklescript.scheme dir::dir
  } else {
    Native.scheme dir::dir
  }
};

let env () => Env.create
  /* TODO: this doesn't traverse down to _build so I can't ask it to clean files there? */
  /* artifacts::(
       fun dir::dir => {
         print_endline @@ (Path.to_string dir ^ "00000000000000000");
         /* if (dir == buildDir || Path.is_descendant dir::dir buildDir) {
           Dep.glob_listing (Glob.create dir::buildDir "*.cmi")
         } else { */
           Dep.return []
         /* } */
       }
     ) */
  scheme;
/* let setup () => Deferred.return env; */
