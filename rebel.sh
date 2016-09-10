# Convert reason files to ocaml files
# TODO probably should use a for loop

# rm -f src/util.ml && refmt -print ml -parse re src/util.re >> src/util.ml
rm -f src/rules.ml && refmt -print ml -parse re src/rules.re >> src/rules.ml
rm -f src/bsRules.ml && refmt -print ml -parse re src/bsRules.re >> src/bsRules.ml
rm -f src/rebel.ml && refmt -print ml -parse re src/rebel.re >> src/rebel.ml

# recompile oasis just in case _oasis is changed
oasis setup && ./configure && make && cp -f ./rebel.native examples/rebel-project/node_modules/.bin/rebel && cd examples/rebel-project && ./node_modules/.bin/rebel && ./_build/rebel-project/app.out
