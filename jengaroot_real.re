open Async.Std;

open Jenga_lib.Api;

open Core.Std;

let ( *>>| ) = Dep.map;

let bash dir::dir command => Action.process dir::dir prog::"bash" args::["-c", command] ();

let bashf dir::dir fmt => ksprintf (fun str => bash dir::dir str) fmt;

/* Same concept as previous gist, but out we're outputting to _build/outout.re, which erroneously still
   gets picked up by the src/ glob */
let scheme dir::dir => {
  let srcDir = Path.root_relative "src";
  let buildDir = Path.root_relative "_build";
  let out = Path.relative dir::buildDir "outout.re";
  Scheme.all [
    Scheme.rules_dep (
      Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>| (
        fun paths => {
          /* Printing all the globbed results. outout.re is being captured here. */
          /* List.map paths f::Path.to_string |> List.iter f::print_endline;
          print_endline @@ (Path.to_string dir ^ "================"); */
          /* As a temporarily workaround, I can filter out _build/outout.re. If you uncomment next line there'd be no more
          cycles */
          let paths = List.filter paths f::(fun p => not (Path.is_descendant dir::buildDir p));
          [
            Rule.simple
              targets::[out]
              deps::(List.map paths f::Dep.path)
              action::(bashf dir::Path.the_root "echo hiasdasd > %s" (Path.to_string out))
          ]
        }
      )
    ),
    Scheme.rules [Rule.default dir::Path.the_root [Dep.path out]]
  ]
};

let env = Env.create scheme;

let setup () => Deferred.return env;
