
open Base
open Lexer
open Parser
open Test
open Debug

let unwrap = function Success(Some(data), _) -> data | _ -> raise (TestFailure "unwrap failed")

;; 

test "parser with single rule single case and single atom pattern" (fun _ ->
    let lexer : lexer = Lexer( "lex", [Rule( "[a]", Atom "A" )] ) in
    let parser : parser = Parser( "parse", [Rule( "main", [Case( [Atom "A"], Atom( "output" ) )] ) ] ) in

    let tokens = lex lexer "a" in
    let output = parse [lexer] [parser] tokens "parse" in

    let data = unwrap output in

    expect_eq data (Atom( "output", meta 0 0 )) "should produce atom output"
)

;; 

test "parser with single rule single case and double atom pattern" (fun _ ->
    let lexer : lexer = Lexer( "lex", [Rule( "[a]", Atom "A" )] ) in
    let parser : parser = Parser( "parse", [Rule( "main", [Case( [Atom "A"; Atom "A"], Atom( "output" ) )] ) ] ) in

    let tokens = lex lexer "aa" in
    let output = parse [lexer] [parser] tokens "parse" in

    let data = unwrap output in

    expect_eq data (Atom( "output", meta 0 1 )) "should produce atom output"
)

;; 

test "parser with single rule single case and wild card pattern" (fun _ ->
    let lexer : lexer = Lexer( "lex", [Rule( "[a]", Atom "A" )
                                      ;Rule( "[b]", Atom "B" )
                                      ] ) in
    let parser : parser = Parser( "parse", [Rule( "main", [Case( [WildCard], Atom( "output" ) )] ) ] ) in

    let t1 = lex lexer "a" in
    let o1 = parse [lexer] [parser] t1 "parse" in
    let d1 = unwrap o1 in

    let t2 = lex lexer "b" in
    let o2 = parse [lexer] [parser] t2 "parse" in
    let d2 = unwrap o2 in

    expect_eq d1 (Atom( "output", meta 0 0 )) "should produce atom output with 'a' input"
    ;
    expect_eq d2 (Atom( "output", meta 0 0 )) "should produce atom output with 'b' input"
)

;; 

test "parser with single rule single case and variable pattern" (fun _ ->
    let lexer : lexer = Lexer( "lex", [Rule( "[a]", Atom "A" )
                                      ;Rule( "[b]", Atom "B" )
                                      ] ) in
    let parser : parser = Parser( "parse", [Rule( "main", [Case( [Var "var_1"], Var( "var_1" ) )] ) ] ) in

    let t1 = lex lexer "a" in
    let o1 = parse [lexer] [parser] t1 "parse" in
    let d1 = unwrap o1 in

    let t2 = lex lexer "b" in
    let o2 = parse [lexer] [parser] t2 "parse" in
    let d2 = unwrap o2 in

    expect_eq d1 (Atom( "A", meta 0 0 )) "should produce atom output with 'a' input"
    ;
    expect_eq d2 (Atom( "B", meta 0 0 )) "should produce atom output with 'b' input"
)

;;

test "parser with single rule single case and two variable pattern" (fun _ ->
    let lexer : lexer = Lexer( "lex", [Rule( "[a]", Atom "A" )
                                      ;Rule( "[b]", Atom "B" )
                                      ] ) in
    let parser : parser = Parser( "parse", [Rule( "main", [Case( [Var "var_1"; Var "var_2"]
                                                               , Fun( "and", [Var( "var_1" )
                                                                             ;Var( "var_2" )] )
                                                               ) ] ) ] ) in

    let t1 = lex lexer "ab" in
    let o1 = parse [lexer] [parser] t1 "parse" in
    let d1 = unwrap o1 in

    let t2 = lex lexer "ba" in
    let o2 = parse [lexer] [parser] t2 "parse" in
    let d2 = unwrap o2 in

    expect_eq d1 (Fun( "and", [Atom( "A", meta 0 0 )
                              ;Atom( "B", meta 1 1 )
                              ], meta 0 1 )) "should produce fun output with 'ab' input"
    ;
    expect_eq d2 (Fun( "and", [Atom( "B", meta 0 0 )
                              ;Atom( "A", meta 1 1 )
                              ], meta 0 1 )) "should produce fun output with 'ba' input"
)

;;

test "parser with single rule single case and single function pattern" (fun _ ->
    let lexer : lexer = Lexer( "lex", [Rule( "[a]", Fun( "fun", []) )]) in
    let parser : parser = Parser( "parse", [Rule( "main", [Case( [Fun( "fun", [] )], Atom "output")] )]) in 
                                                               
    let t = lex lexer "a" in
    let o = parse [lexer] [parser] t "parse" in
    let d = unwrap o in

    expect_eq d (Atom( "output", meta 0 0)) "should produce atom output from input"
)

;;

test "parser with single rule single case and single function(atom) pattern" (fun _ ->
    let lexer : lexer = Lexer( "lex", [Rule( "[a]", Fun( "fun", [Atom "A"]) )]) in
    let parser : parser = Parser( "parse", [Rule( "main", [Case( [Fun( "fun", [Atom "A"] )], Atom "output")] )]) in 

    let t = lex lexer "a" in
    let o = parse [lexer] [parser] t "parse" in
    let d = unwrap o in

    expect_eq d (Atom( "output", meta 0 0)) "should produce atom output from input"
)
(* 
    capture group reference
    nested capture group reference
    variable nested inside function works
    parse reference
    rule reference
    function match
    function match with atom
    function match with multiple atoms
    function match with group reference (also nested)
    function match with parse reference
    function match with rule reference
    multiple cases
    nothing pattern match
    nothing construction
    final index is correct
*)
