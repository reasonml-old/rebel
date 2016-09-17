open Core.Std;

List.iter [1, 2, 3] f::(fun x => print_endline (string_of_int x));

print_endline Test1.secret;

open Remath.Index;

print_endline (string_of_int (fibonacci 1));
print_endline (string_of_int (fibonacci 2));
print_endline (string_of_int (fibonacci 3));
