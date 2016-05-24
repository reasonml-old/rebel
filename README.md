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

**To run**: `jenga`. Then see the output from `_build/top/app.out`, and/or open up `index.html` to see the compiled-to-js version.

**To develop the jengaroot yourself**: have all the [Reason](https://github.com/facebook/reason) toolchain installed. Compile to jengaroot.ml. Here's a convenient compile & prettify command I've temporarily included in my bashrc/zshrc:

```sh
esc=$(printf '\033')
alias f="refmt -parse re -print ml jengarootReal.re \
  | sed 's/\[@explicit_arity \]//g' \
  > jengaroot.ml \
  && (time jenga -show-actions-run-verbose -verbose -show-checked \
  -show-error-dependency-paths -brief-error-summary -show-buildable-discovery \
  -show-reflecting -show-considering -trace) | \
  2>&1 \
  sed 's/exit_code = 0/${esc}[32m&${esc}[39m/' \
  | sed 's/exit_code = 1/${esc}[31m&${esc}[0m/' \
  | sed 's/\*\*\* /${esc}[30m> ${esc}[0m/' \
  | sed 's/jenga: ERROR:/${esc}[31m&${esc}[0m/' \
  | sed 's/- build/${esc}[30m> ${esc}[0m${esc}[40m${esc}[33mbuild${esc}[0m/' \
  | sed 's/+ bash/${esc}[30m> ${esc}[0m${esc}[40m${esc}[33mbash${esc}[0m/' \
  | sed 's/- exit.*/${esc}[30m> ${esc}[30m&${esc}[0m/' \
  | sed 's/jenga: NOT RUNNING:/${esc}[40m${esc}[34m&${esc}[0m/' \
  | sed 's/jenga: Considering:.*/${esc}[30m&${esc}[0m/' \
  | sed 's/jenga: Building:/${esc}[0m${esc}[40m${esc}[33m&${esc}[0m/' \
  | sed 's/jenga: //' \
  | sed 's/bash -c.*/${esc}[3m&${esc}[0m/' \
  | huh"
```

(The last `huh` is exposed from [BetterErrors](https://github.com/chenglou/BetterErrors). If you don't have it, just remove that part.)

**Things to test**
- Support for source/interface interface files that are (uncapitalized, Capitalized) and (snake_cased, camelCased). Support third-party library names of the latter pair (npm modules can no longer be capitalized).
- Compiles dangling unused source files and unused dependencies (for merlin editor integration, e.g. add a new dependency, compile, then start autocompleting that previously unused module).
- Dependencies that contain the same source file names.
- Third-party sources with unique names.
- Compiling A and B that both require C.
- .rei interface files referencing third-party modules that source files don't (extra dependencies).
- `open`ing a third-party library to refer to its module that happens to have the same name as one of our first-party modules.
