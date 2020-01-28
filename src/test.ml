
open Base
open Lexer
open Parser
open Debug

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

;;

test "lexer with single atom rule" (fun _ ->
    let lexer : lexer = Lexer( "a", [Rule( "[a]", Atom "A" )] ) in

    let data = lex lexer "aaa" in

    expect_eq data [ Atom( "A", meta 0 0 )
                   ; Atom( "A", meta 1 1 )
                   ; Atom( "A", meta 2 2 )
                   ] "should produce AAA" 
)

;; 

test "lexer with single fun rule" (fun _ ->
    let lexer : lexer = Lexer( "a", [Rule( "[a]", Fun("F", [Atom "A"; Atom "B"]) )] ) in

    let data = lex lexer "aaa" in

    expect_eq data [ Fun( "F", [Atom( "A", meta 0 0 ); Atom( "B", meta 0 0 )], meta 0 0 )
                   ; Fun( "F", [Atom( "A", meta 1 1 ); Atom( "B", meta 1 1 )], meta 1 1 )
                   ; Fun( "F", [Atom( "A", meta 2 2 ); Atom( "B", meta 2 2 )], meta 2 2 )
                   ] "should process F(AB) F(AB) F(AB)" 
)

;;

test "lexer with single nothing rule" (fun _ ->
    let lexer : lexer = Lexer( "a", [Rule( "[a]", Nothing )] ) in

    let data = lex lexer "aaa" in

    expect_eq data [] "should process Nothing" 
)

;;

test "lexer with single capture rule" (fun _ ->
    let lexer : lexer = Lexer( "a", [Rule( "[a]", CaptureRef [0] )] ) in

    let data = lex lexer "aaa" in

    expect_eq data [ String("a", meta 0 0)
                   ; String("a", meta 1 1)
                   ; String("a", meta 2 2)
                   ] "should process \"aaa\"" 
)

;;

test "lexer with two atom rules" (fun _ ->
    let lexer : lexer = Lexer( "ab", [Rule( "[a]", CaptureRef [0] )
                                     ;Rule( "[b]", CaptureRef [0] )
                                     ] ) in

    let d1 = lex lexer "aaa" in
    let d2 = lex lexer "bbb" in
    let d3 = lex lexer "bab" in
    let d4 = lex lexer "" in

    expect_eq d1 [ String("a", meta 0 0)
                 ; String("a", meta 1 1)
                 ; String("a", meta 2 2)
                 ] "should process \"aaa\"" 

    ;

    expect_eq d2 [ String("b", meta 0 0)
                 ; String("b", meta 1 1)
                 ; String("b", meta 2 2)
                 ] "should process \"bbb\"" 

    ;

    expect_eq d3 [ String("b", meta 0 0)
                 ; String("a", meta 1 1)
                 ; String("b", meta 2 2)
                 ] "should process \"bab\"" 

    ;

    expect_eq d4 [] "should process \"\"" 
)
