#!/usr/local/bin/node
'use strict';

let execSync = require('child_process').execSync;
let path = require('path');

let libName = process.argv[2];
let firstPartySourceFilePaths = process.argv.slice(3);

function capitalize(str) {
  return str[0].toUpperCase() + str.slice(1);
}

let wow = firstPartySourceFilePaths
  .map((f) => {
    let fName = path.parse(f).name;
    return `let module ${capitalize(fName)} = ${capitalize(libName)}__${fName};`
  })
  .join(' ');

console.log(`: \
  |> ^o^ echo "${wow}" > _build/${libName}/${libName}.re \
  |> _build/${libName}/${libName}.re`);

console.log(`: _build/${libName}/${libName}.re \
  |> ocamlc -pp refmt -g -no-alias-deps -w -49 -c -impl _build/${libName}/${libName}.re -o _build/${libName}/${libName}.cmo \
  |> _build/${libName}/${libName}.cmo _build/${libName}/${libName}.cmi`);
