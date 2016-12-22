rm -rf _build/src

mkdir -p _build/src

selfSortedFiles=$(ocamldep -pp refmt -sort -ml-synonym .re src/*.re)

echo $selfSortedFiles

for source in $selfSortedFiles
do
  destination=$(echo $source | sed "s/src/_build\/src/" | sed "s/\.re$//")
  ocamlfind ocamlc -custom -pp refmt -w "-40-30" -thread -package ppx_let -package core -package jenga -package yojson -o $destination -I _build/src -c -impl $source
done


selfSortedArtifacts=$(echo $selfSortedFiles | sed "s/src/_build\/src/g" | sed "s/\.re/\.cmo/g")
ocamlfind ocamlc -custom -o _build/src/rebel -linkpkg -thread -package ppx_let -package core -package jenga -package yojson -I _build/src $selfSortedArtifacts
