#!/usr/local/bin/node
'use strict';

let execSync = require('child_process').execSync;
let path = require('path');
let fs = require('fs');

let asd = fs.readdirSync('./node_modules/bookshop');
// console.log(asd);
// first: node, second: script name
let files = process.argv.slice(2);
let filesWithoutDummy = files.filter((f) => {return f.indexOf('.txt') >= 0});
let ww = filesWithoutDummy.join(' ');
console.log(`: |> cat ${ww} > fuckballs.txt |> fuckballs.txt`);




// first: node, second: script name
let files = process.argv.slice(2);
let libName = 'hi';
let buildDir = '_build/' + libName + '/';

let filesTopologicalOrder = execSync(
  'ocamldep -pp refmt -sort ' + files.map((f) => {return ' -impl ' + f;}).join(' '),
  {encoding: 'utf-8'}
).trim();

let filesRequires = execSync(
  'ocamldep -pp refmt -modules ' + filesTopologicalOrder.split(' ').map((f) => {return ' -impl ' + f;}).join(' '),
  {encoding: 'utf-8'}
).trim();

function uncapitalize(str) {
  return str[0].toLowerCase() + str.slice(1);
}

function capitalize(str) {
  return str[0].toUpperCase() + str.slice(1);
}

let wow = files
  .map((f) => {
    let fName = path.parse(f).name;
    return `let module ${capitalize(fName)} = ${capitalize(libName)}__${fName};`
  })
  .join(' ');

console.log(`: \
  |> ^o^ echo "${wow}" > _build/hi/hi.re \
  |> _build/hi/hi.re`);

console.log(`: _build/hi/hi.re \
  |> ocamlc -pp refmt -g -no-alias-deps -w -49 -c -impl _build/hi/hi.re -o _build/hi/hi.cmo \
  |> _build/hi/hi.cmo _build/hi/hi.cmi`);

filesRequires.split(/\n/).forEach((fileAndDeps) => {
  let asd = fileAndDeps.split(':');
  let file = asd[0].trim();
  let rawDeps = asd[1].trim()
  let deps = rawDeps === '' ? [] : rawDeps.split(' ');
  let tupDeps = deps.map((d) => {return buildDir + libName + '__' + uncapitalize(d) + '.cmi';}).join(' ');
  let filePathWithoutExt = buildDir + libName + '__' + path.parse(file).name;
  console.log(`: _build/hi/hi.cmi ${tupDeps} \
  |> ocamlc -pp refmt -g -open Hi -I ${buildDir} -o ${filePathWithoutExt}.cmo -intf-suffix rei -c -impl ${file} \
  |> ${filePathWithoutExt}.cmo ${filePathWithoutExt}.cmi`);
});

let entryCmoDeps = filesTopologicalOrder
  .split(' ')
  .map((f) => {
    return buildDir + libName + '__' + path.parse(f).name + '.cmo';
  })
  .join(' ');
console.log(`: ${buildDir}*.cmo \
  |> ocamlc -I ${buildDir} -o ${buildDir}${libName}__main.out ${buildDir}${libName}.cmo ${entryCmoDeps} \
  |> ${buildDir}${libName}__main.out`);
