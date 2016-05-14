let x = 1000;

let module NestedInC = {
  type t = int;
  let nestedX = 1;
  let module NestedSoNestedInC = {
    let theNestest = 2;
  };
};
