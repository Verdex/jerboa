
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

(* 
    TODO : go ahead and check that there are no variable collisions ... environments will 
           probably be small enough that it won't be a problem
*)
let env_merge (e1 : (string * data) list) (e2 : (string * data) list) : (string * data) list =
    []

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

    let rec match_pattern (ps : pattern list) 
                      (input : data list) 
                      (index : int) : (data list * (string * data) list * int) option =

        let rec m (ts : (pattern * data) list) i cap_list env =
            match ts with
            | [] -> Some(List.rev cap_list, env, i) 

            | (Atom(p_name), (Atom(d_name, _) as d)) :: r when p_name = d_name 
                -> m r (i + 1) (d :: cap_list) env

            | (Atom(_), _) :: _ -> None

            | (Fun(p_name, p_params), Fun(d_name, d_params, meta)) :: r when p_name = d_name
                -> (
                   match match_pattern p_params d_params 0 with
                   | None -> None
                   | Some(sub_cap, sub_env, _) 
                     -> m r (i + 1) (Fun(d_name, sub_cap, meta) :: cap_list) (env_merge sub_env env)
                   )

            | (Fun(_, _), _) :: _ -> None 

            | (WildCard, d) :: r -> m r (i + 1) (d :: cap_list) env 

            | (Var name, d) :: r -> m r (i + 1) (d :: cap_list) (env_merge [(name, d)] env)

            | _ -> None
            (*
            | RuleRef of string
            | ParserRef of string * string*)
        in

        let sub_list = sub input index in

        match List.length sub_list < List.length ps with
        | true -> None
        | false -> let targets = zip ps sub_list in m targets index [] []

    and try_cases (cases : parser_case) (input : data list) : data option = 
        Some( Atom("TODO", {start_index=0; end_index=0}) )
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
