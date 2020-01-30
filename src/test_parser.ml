
open Base
open Lexer
open Parser
open Test
open Debug

;; 

test "parser with single rule single case and single atom pattern" (fun _ ->
    let lexer : lexer = Lexer( "lex", [Rule( "[a]", Atom "A" )] ) in
    let parser : parser = Parser( "parse", [Rule( "main", [Case( [Atom "A"], Atom( "output" ) )] ) ] ) in

    let tokens = lex lexer "a" in
    let output = parse [lexer] [parser] tokens "parse" in

    let Success(Some(data), _) = output in

    expect_eq data (Atom( "output", meta 0 0 )) "should produce atom output"
)

(* 
    wild card
    variable
    capture group reference
    nested capture group reference
    parse reference
    rule reference
    atom match
    function match
    function match with atom
    function match with multiple atoms
    function match with group reference (also nested)
    function match with parse reference
    function match with rule reference
    multiple cases
    nothing pattern match
    nothing construction
*)
