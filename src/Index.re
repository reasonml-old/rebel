open C;
open C.NestedInC;
print_int (A.x + B.x + x + NestedInC.nestedX + NestedSoNestedInC.theNestest);
print_endline "in main";

open Bookshop;
let commonDep = Chenglou.Index.age;
let commonDep2 = Index.commonDep;
let commonDep3 = ZyBook.a;

Js.Unsafe.js_expr "document.write('assd')";
