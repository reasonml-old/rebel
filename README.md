# Jengaboot

Prototype that conforms to the ideal [Reason](https://github.com/facebook/reason) build spec found [here](https://github.com/facebook/reason/wiki/Reason-Project:-Proposal-For-Unifying-Local-Development-And-Package-Manement) and [here](https://github.com/facebook/reason/wiki/The-Ideal-Package-Sandbox). Implemented with [Jenga](https://github.com/janestreet/jenga), Jane Street's all-purpose build system.

## Features

- Lightning build speed: fast startup, incremental, parallel, etc.
- `./node_modules/.bin/compile` to build everything (shorter command coming soon!). No need for sub-build commands since only changed things are rebuilt.
- Works with npm-style `node_modules` third-party dependencies that follows the spec (see next section).
- Works with Reason and vanilla OCaml syntax, out of the box.
- Generates the correct .merlin files for [Merlin](https://github.com/the-lambda-church/merlin).
- Generates JavaScript output through [js_of_ocaml](http://ocsigen.org/js_of_ocaml/).
- Peace-of-mind "wipe the whole world" : just remove `_build/`.
- Works with OPAM/ocamlfind dependencies: put them in the `jengaboot.ocamlfindDependencies` field, [like so](https://github.com/chenglou/jengaboot/blob/e4a8860617b1c27f0faeeb40082476a22c5e07df/package.json#L28).

**Coming soon**:
- Generates utop/rtop bootstrap file.
- Generates documentation based on `mli`/`rei` interface files.
- Easier installation: bundle Jenga, etc.

## "Ideal Reason Project" Spec

- Needs a `src/`, and it must be a flat directory.
- Third-party libraries go into `node_modules/`.
- Third-party libraries names of the format `foo-bar` are transformed into FooBar when used as module name (source file names aren't allowed to have kebab-case).
- Every library (including the current one) needs a package.json that lists its dependencies, if any.

## For Consumers

You need to have Jenga, YoJson, Ocamlfind and js_of_ocaml installed via OPAM:
- `opam update`
- `opam pin add -y jenga https://github.com/chenglou/jenga.git#2a0eb726f503038ad70d43f8e8bbb4c41223108a`
- `opam install js_of_ocaml`
- `opam install yojson`
- `opam install ocamlfind`
- `npm install --save-dev jengaboot`
- Write some files in your `src/`, or install some compliant (see above) npm packages.
- `./node_modules/.bin/run`

## For Contributors
Bother me on IRC/Twitter/issues/etc. for specific details.

This repo's structure is a bit weird but convenient. The actual development of the jengaroot logic done in `node_modules/jengaboot`, and the whole repo simulates a magic spec compliant use-case of the jengaboot.

- Do the previous sections' installs up until (including) yojson
- `git clone` this repo
- `npm install`
- modify jengaroot logic in `node_modules/jengaboot/jengaroot.re` (`jengaroot.ml` is the compiled result; the step below takes care of that automatically)
- modify package.json parsing logic in `node_modules/jengaboot/buildUtils/*`, then run `npm run compileUtils` in `node_modules/jengaboot` (check that directory's readme and the `compileUtils` command itself. They're self-explanatory.)
- `./test.sh`
- `_build/top/app.out` to see output, or open `index.html` to see output in console & on screen

Publishing to npm is done inside `node_modules/jengaboot`. Don't do this yourself for now.

### Things to test
- Support for source/interface interface files that are (uncapitalized, Capitalized) and (snake_cased, camelCased). Support third-party library names of the latter pair (npm modules can no longer be capitalized).
- Compiles dangling unused source files and unused dependencies (for merlin editor integration, e.g. add a new dependency, compile, then start autocompleting that previously unused module).
- Dependencies that contain the same source file names.
- Third-party sources with unique names.
- Compiling A and B that both require C.
- .rei interface files referencing third-party modules that source files don't (extra dependencies).
- `open`ing a third-party library to refer to its module that happens to have the same name as one of our first-party modules.
- Compile libraries with mixed `re`, `rei`, `ml` and `mli` files.
- Symlinks.
- Third-party folders with kebab-case, snake_case, camelCase and CamelCase.
- Not compiling unused dependencies.

## Credits
The whole Jane Street team for making Jenga and helping me understanding it. Also to the whole Reason team of course.
