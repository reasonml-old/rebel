open Core.Std;

List.iter f::(fun x => print_endline (string_of_int x)) Test1.numbers;

print_endline Test1.secret;

open Remath.Index;

print_endline (string_of_int (fibonacci 1));
print_endline (string_of_int (fibonacci 2));
print_endline (string_of_int (fibonacci 3));
