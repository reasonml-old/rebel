/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let f () => "Hello World, Compiled from REBEL!!";

let g () => Test1.my_secret;

let a = Rexpress.Express.a;

open Rexpress.Express;

let __dirname: Js.undefined string = [%bs.node __dirname];

let app = Express.express ();

Express.get app "/" (fun req res => Response.json res [%bs.obj {root: __dirname}]);

Express.use app (Express.static path::"__dirname");

Express.listen app 3000;
