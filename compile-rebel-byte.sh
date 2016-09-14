rm -rf _build/src

mkdir -p _build/src

selfSortedFiles=$(ocamldep -pp refmt -sort -ml-synonym .re src/*.re)

echo $selfSortedFiles

for source in $selfSortedFiles
do
  destination=$(echo $source | sed "s/src/_build\/src/" | sed "s/\.re$//")
  ocamlfind ocamlc -g -bin-annot -pp refmt -w "-40-30" -thread -package threads -package ppx_let -package core -package jenga -package yojson -o $destination -I _build/src -c -impl $source
done


selfSortedArtifactsO=$(echo $selfSortedFiles | sed "s/src/_build\/src/g" | sed "s/\.re/\.cmo/g")
selfSortedArtifactsX=$(echo $selfSortedFiles | sed "s/src/_build\/src/g" | sed "s/\.re/\.cmx/g")
# should give: _build/self/myDep.cmo _build/self/myDep2.cmo _build/self/test.cmo
# ocamlfind ocamlc -o _build/src/rebel -thread -package threads -package ppx_let -package core -package jenga -package yojson -I _build/src $selfSortedArtifactsO
ocamlfind ocamlc -o _build/src/rebel -linkpkg -thread -package threads -package ppx_let -package core -package jenga -package yojson -I _build/src $selfSortedArtifactsO
