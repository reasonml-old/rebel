print_int (A.x + B.x);

print_endline "in main";

let commonDep = Chenglou.Test.age;
let commonDep2 = Bookshop.Test.commonDep;
let commonDep3 = Bookshop.MyBook.a;

print_int (commonDep + commonDep2 + commonDep3);
