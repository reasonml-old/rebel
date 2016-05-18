# Jengaboot

Prototype that conforms to the magic-build spec. Implemented with Jenga.

_Really dirty code right now, refactoring soon._

Current directory structure:

```
├── README.md
├── jengaroot_real.re
├── jengaroot.ml (compiled from jengaroot_real.re through [censored technology])
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
    ├── Index.re
    └── Test.re
```

As of today, works on latest locally pinned jenga, ocaml v4.02.3, and whatever `opam update` dependencies versions.

## Restrictions
Most of these are for the ease of the prototype, but they might or might not be temporary.

- Only works with [censored] source files for now.
- Needs a `src/`.
- `src/` must be flat.
- Source files must be upper-cased.
- Entry file must be called `Index.re`.
- Third-party deps go into `node_modules/`.
- Third-party deps names are uncapitalized (can contain upper-case in the whole word).
- Don't open a module and refer to its inner module that has the same name as the current file (this causes some false positive cycle detection. Will fix later).

**To run**: `jenga`.

**To develop the jengaroot yourself**: have all the [censored] toolchain installed. Compile to jengaroot.ml. Here's a convenient compile & prettify command I've temporarily included in my bashrc/zshrc:

```sh
esc=$(printf '\033')
alias f="refmt -parse re -print ml jengaroot_real.re \
  | sed -E 's/\[@explicit_arity \]//g' \
  > jengaroot.ml \
  && (time jenga -show-actions-run-verbose -verbose -show-checked \
  -show-error-dependency-paths -brief-error-summary -show-buildable-discovery \
  -show-considering -show-reconsidering -show-reflecting -trace) | \
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
  | sed 's/jenga: //' \
  | sed 's/bash -c.*/${esc}[3m&${esc}[0m/' \
  | huh"
```

(The last `huh` is exposed from [BetterErrors](https://github.com/chenglou/BetterErrors). If you don't have it, just remove that part.)
