/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

let module Scheme = Jenga_lib.Api.Scheme;

let module Env = Jenga_lib.Api.Env;

open Utils;

let scheme dir::dir => Scheme.all [
  Ocaml.scheme dir::dir,
  Bucklescript.scheme dir::dir,
  Merlin.scheme dir::dir
];

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
