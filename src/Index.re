/* this is a reason source file */
open C;
open C.NestedInC;
print_int (A.x + B.x + x + NestedInC.nestedX + NestedSoNestedInC.theNestest);
print_endline "in main";

let commonDep5 = VeryCommonName.top;
let commonDep = Chenglou.VeryCommonName.age;
open Bookshop;
let commonDep2 = Index.commonDep;
let commonDep3 = ZyBook.a;
let commonDep4 = VeryCommonName.z;

Js.Unsafe.js_expr "document.write('assd')";
