/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

open Jenga_lib.Api;

open Utils;

let scheme dir::dir => {
  ignore dir;
  let backend = rebelConfig.backend;
  let merlinScheme = rebelConfig.merlin ? Merlin.scheme dir::dir : Scheme.no_rules;
  switch backend {
  | "bucklescript" => Scheme.all [Bucklescript.scheme dir::dir, merlinScheme]
  | "jsoo"
  | "byte"
  | "native" => Scheme.all [Ocaml.scheme dir::dir, merlinScheme]
  | _ =>
    print_endline "Invalid backend it should be one of [ native, jsoo, bucklescript ]";
    Scheme.no_rules
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
