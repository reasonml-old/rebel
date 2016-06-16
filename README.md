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

**Coming soon**:
- Generates utop/rtop bootstrap file.
- Generates documentation based on `mli`/`rei` interface files.
- Works with OPAM dependencies.
- Easier installation: bundle Jenga, etc.

## "Ideal Reason Project" Spec

- Needs a `src/`, and it must be a flat directory.
- Third-party libraries go into `node_modules/`.
- Third-party libraries names of the format `foo-bar` are transformed into FooBar when used as module name (source file names aren't allowed to have kebab-case).
- Every library (including the current one) needs a package.json that lists its dependencies, if any.

## For Consumers

You need to have Jenga, YoJson and js_of_ocaml installed via OPAM:
- `opam update`
- `opam pin add -y jenga https://github.com/chenglou/jenga.git#2a0eb726f503038ad70d43f8e8bbb4c41223108a`
- `opam install js_of_ocaml`
- `opam install yojson`
- `npm install --save-dev jengaboot`
- Write some files in your `src/`, or install some compliant (see above) npm packages.
- `./node_modules/.bin/run`


## For Contributors
Bother me on IRC/Twitter/issues/etc. for specific details.

- Do the previous sections' installs up until (including) yojson
- `git clone` this repo
- `npm install`
- `jenga`
- `_build/top/app.out` to see output, or open `index.html` to see output in console & on screen

**To develop the jengaroot yourself**: have all the [Reason](https://github.com/facebook/reason) toolchain installed. Compile jengarootReal.re to jengaroot.ml. Here's a convenient compile & prettify command I've temporarily included in my bashrc/zshrc:

```sh
esc=$(printf '\033')
alias f="refmt -parse re -print ml jengarootReal.re \
  | sed 's/\[@explicit_arity \]//g' \
  > jengaroot.ml \
  && time jenga"
```

**Things to test**
- Support for source/interface interface files that are (uncapitalized, Capitalized) and (snake_cased, camelCased). Support third-party library names of the latter pair (npm modules can no longer be capitalized).
- Compiles dangling unused source files and unused dependencies (for merlin editor integration, e.g. add a new dependency, compile, then start autocompleting that previously unused module).
- Dependencies that contain the same source file names.
- Third-party sources with unique names.
- Compiling A and B that both require C.
- .rei interface files referencing third-party modules that source files don't (extra dependencies).
- `open`ing a third-party library to refer to its module that happens to have the same name as one of our first-party modules.
- compile libraries with mixed `re`, `rei`, `ml` and `mli` files.
- Symlinks.
- third-party folders with kebab-case, snake_case, camelCase and CamelCase.

## Credits
The whole Jane Street team for making Jenga and helping me understanding it. Also to the whole Reason team of course.
