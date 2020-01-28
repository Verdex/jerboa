
open Base

exception TestFailure of string

let test (name : string) (t : unit -> unit) =
    try 
        t () 
        ; Printf.printf "%s\n" (String.concat "" [name ; ": pass"])
    with 
    | TestFailure v -> Printf.printf "%s\n" (String.concat "" [name ; ": failed with message " ; v])
    | _ -> Printf.printf "%s\n" (String.concat "" [name ; ": failed with exception"])


let expect_eq (a : 'a) (b : 'a) (s : string) =
    match a = b with
    | true -> ()
    | false -> raise (TestFailure s)

let meta s e = { start_index = s ; end_index = e }


