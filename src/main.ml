
open Base
open Construction
open Util
open Lexer
open Debug (* todo *)


(* todo *)
(* data_to_lexer, data_to_parser *)
let to_eval (input:data) : string = "todo"

exception LexerNotFound of string
exception ParserNotFound of string
exception RuleNotFound of string

type match_result = 
    | Failure
    | Success of int
    | SuccessWithData of data * int
    | SuccessWithNamedData of data * string * int

let gen (lexers : lexer list) (parsers : parser list) =

    let find_lexer name = 
        try List.find (function (Lexer(n, _)) -> n = name) lexers 
        with Not_found -> raise (LexerNotFound name) 
    in

    let find_parser name = 
        try List.find (function (Parser(n, _)) -> n = name) parsers
        with Not_found -> raise (ParserNotFound name) 
    in

    let find_parse_rule rules name = 
        try List.find (function (Rule(n, _) : parser_rule) -> n = name) rules
        with Not_found -> raise (RuleNotFound name) 
    in

    let single_match (p : pattern) (input : data list) index : match_result =

        let requires_data f = 
            match index < List.length input with
            | true -> f (l_nth input index)  
            | false -> Failure
        in

        let atom_match : string -> data -> match_result = 
            fun name data -> 
                match data with
                | Atom(n,_) as d when n = name -> SuccessWithData(d, index + 1)
                | _ -> Failure
        in

        match p with
        | Atom(name) -> requires_data (atom_match name)
        | Fun(name, params) -> Failure 
        | Var(name) -> requires_data (fun data -> SuccessWithNamedData(data, name, index + 1))
        | WildCard -> requires_data (fun data -> SuccessWithData(data, index + 1))
        | RuleRef(rule_name) -> Failure 
        | ParserRef(lexer, parser) -> Failure 
        | Nothing -> Success index 
    in

    let match_pattern (ps : pattern list) 
                      (c : constructor)
                      (input : data list) 
                      (index : int) : (data list * (string * data) list * int) option =
        None
    in

    let try_cases (cases : parser_case) (input : data list) : data = 
        Atom("TODO", {start_index=0; end_index=0})
    in

    (find_parser, try_cases)

(* todo *)
let parse (lexers : lexer list) 
          (parsers : parser list)
          (input : data list)
          (initial_parser_name : string) : data =

    let (find_parser, try_cases) = gen lexers parsers in

    let initial_parser = find_parser initial_parser_name in 

    Atom( "todo", {start_index = 0; end_index = 0})

let l = Lexer("lexer", [ Rule("[a]", Atom "FoundA")
                       ; Rule("[b]", Fun("FoundB", [CaptureRef([0])]))
                       ; Rule("[c]+", Fun("foundC", [CaptureRef([0])]))
                       ] )


;; let output = lex l "ccc  a\tb \n"

(* ;; let output = construct (CaptureRef([1;1;0])) 
                        [Atom("ikky", {start_index=5; end_index=9})
                        ; Fun("blah", [Atom("inner1", {start_index=0; end_index=1})
                                      ;Fun("inner2", [ Atom("inner inner 1", {start_index=0; end_index=2})
                                                     ], {start_index=0; end_index=0})
                                      ], {start_index=10; end_index=11})] [] *)
                                      
;; List.iter (fun o -> print_data o) output
