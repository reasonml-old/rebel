# Jengaboot

Prototype that conforms to the magic-build spec. Implemented with Jenga.

Approximate directory structure:

```
├── README.md
├── jengaroot_real.re
├── jengaroot.ml (compiled from jengaroot_real.re through Reason)
├── node_modules (simulated third-party deps)
│   ├── bookshop (this also depends on chenglou, a dep shared by top level library)
│   │   ├── package.json (simulated library metadata)
│   │   └── src
│   │       ├── myBook.re
│   │       ├── test.re
│   │       └── test.rei
│   └── chenglou
│       ├── package.json
│       └── src
│           └── test.re
└── src
    ├── a.re
    ├── b.re
    ├── c.re
    ├── main.re
    └── test.re
```

As of today, works on latest locally pinned jenga, ocaml v4.02.3, and whatever `opam update` dependencies versions for jenga.

## Restrictions
Most of these are for the ease of the prototype, but they might or might not be temporary.

- Only works with Reason source files for now.
- Needs a `src/`, and it must be a flat directory.
- Third-party deps go into `node_modules/`.
- Third-party deps names are uncapitalized (can contain upper-case in the whole word).
- Every library (including the current top one) needs a package.json that lists its dependencies, if any.

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

## Credits
The whole Jane Street team for making Jenga and helping me understanding it. Also to the whole Reason team of course.
