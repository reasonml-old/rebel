open Yojson.Basic;

let packageJsonPath = Sys.argv.(1);

let backends =
  from_file packageJsonPath |>
    Util.member "rebel" |> Util.to_option (fun a => a |> Util.member "backend");

switch backends {
| Some (`List _ as a) => List.map Util.to_string (Util.to_list a)
| None => [ "native", "javascript" ]
| _ => []
} |> String.concat "\n" |> print_endline;
