#!/usr/local/bin/node
'use strict';

let execSync = require('child_process').execSync;
let path = require('path');

// first: node, second: script name
let inputPaths = process.argv.slice(2);
let libName = 'hi';
let buildDir = '_build/' + libName + '/';

let firstPartySourceFilePaths = inputPaths.filter((p) => {
  return p.indexOf('src/') >= 0;
});

let nodeModulesDirs = inputPaths.filter((p) => {
  return p.indexOf('src/') === -1 && p.indexOf('dummy.txt') === -1;
});

function filesTopologicalOrder(sourceFilePaths) {
  return execSync(
    'ocamldep -pp refmt -sort ' + sourceFilePaths.map((f) => {return ' -impl ' + f;}).join(' '),
    {encoding: 'utf-8'}
  ).trim().split(' ');
}


function filesDeps(sourceFilePaths) {
  let filesRequires = execSync(
    'ocamldep -pp refmt -modules ' + sourceFilePaths.split(' ').map((f) => {return ' -impl ' + f;}).join(' '),
    {encoding: 'utf-8'}
  ).trim();

  return filesRequires
    .split(/\n/)
    .map((fileAndDeps) => {
      let asd = fileAndDeps.split(':');
      let file = asd[0].trim();
      let rawDeps = asd[1].trim()
      let deps = rawDeps === '' ? [] : rawDeps.split(' ');
      return [file, deps];
    });
}

function pathToModuleName(p) {
  return capitalize(path.parse(file).name);
}

function thirdPartyModules(fileDepsRes) {
  let temporaryExceptions = [
    "Array",
    "List",
    "String",
    "Sys",
    "BetterErrorsMain",
    "Buffer",
    "Filename",
    "Pcre",
    "Printf",
    "Sys",
    "Unix",
    "Yojson",
    "Pcre",
    "BatIO",
    "BatArray",
    "BatIO",
    "BatList",
    "BatString",
    "BatSys",
    "BatTuple"
  ];
  let currLibModuleNames = fileDepsRes.map((f) => {return f[0];}).map(pathToModuleName)
  return Array.prototype.concat.apply([], fileDepsRes.map((a) => {return a[1]}))
    .filter((x) => {
      return temporaryExceptions.indexOf(x) === -1 &&
        currLibModuleNames.indexOf(x) === -1;
    })
};

// function compileAll(isTopLib, libName) {
//   let thirdPartyModules
// }


let filesRequires = execSync(
  'ocamldep -pp refmt -modules ' + firstPartySourceFilePaths.map((f) => {return ' -impl ' + f;}).join(' '),
  {encoding: 'utf-8'}
).trim();

function uncapitalize(str) {
  return str[0].toLowerCase() + str.slice(1);
}

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

let entryCmoDeps = filesTopologicalOrder(firstPartySourceFilePaths)
  .map((f) => {return buildDir + libName + '__' + path.parse(f).name + '.cmo';})
  .join(' ');

console.log(`: ${buildDir}*.cmo \
  |> ocamlc -I ${buildDir} -o ${buildDir}${libName}__main.out ${buildDir}${libName}.cmo ${entryCmoDeps} \
  |> ${buildDir}${libName}__main.out`);
