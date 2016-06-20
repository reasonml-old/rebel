#### Fake it til you make it

Jenga currently doesn't allow a jengaroot build rules file to load external dependencies, e.g. [yojson](https://github.com/mjambon/yojson) for parsing our package.json files. Therefore, we've made two scripts (which will be compiled to executables through an npm `postinstall` hook). One of these takes in package.json and outputs the `dependencies` JavaScript object keys, one per row. The other does the same but reads from `jengaboot.ocamlfindDependencies` instead. Our jengaroot can therefore call the executables and easily "parse" the output.
