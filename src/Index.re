print_int (A.x + B.x + C.x + C.NestedInC.nestedX + C.NestedInC.NestedSoNestedInC.theNestest);

print_endline "in main";

let commonDep = Chenglou.Index.age;
let commonDep2 = Bookshop.Index.commonDep;
let commonDep3 = Bookshop.ZyBook.a;
ignore Test.a;

print_int (commonDep + commonDep2 + commonDep3);
/* open C;
   open C.NestedInC;
   print_int (A.x + B.x + x + NestedInC.nestedX + NestedSoNestedInC.theNestest);
   print_endline "in main";

   open Bookshop;
   let commonDep = Chenglou.Index.age;
   let commonDep2 = Index.commonDep;
   let commonDep3 = ZyBook.a;
   Js.Unsafe.js_expr "document.write('assd')";
   */
