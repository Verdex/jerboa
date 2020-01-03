
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
exception EnvNameCollision of string list

let env_merge (e1 : (string * data) list) (e2 : (string * data) list) : (string * data) list =
    let open List in
    let collisions = map (fun (n1, _) -> filter (fun (n2, _) -> n1 = n2) e2) e1
                   |> concat 
                   |> map (fun (n, _) -> n) in

    if length collisions <> 0 then
        raise (EnvNameCollision collisions)
    ;
    append e1 e2

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

    let rec match_pattern (rules : parser_rule list)
                          (ps : pattern list) 
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
                   match match_pattern rules p_params d_params 0 with
                   | None -> None
                   | Some(sub_cap, sub_env, _) 
                     -> m r (i + 1) (Fun(d_name, sub_cap, meta) :: cap_list) (env_merge sub_env env)
                   )

            | (Fun(_, _), _) :: _ -> None 

            | (WildCard, d) :: r -> m r (i + 1) (d :: cap_list) env 

            | (Var name, d) :: r -> m r (i + 1) (d :: cap_list) (env_merge [(name, d)] env)

            | (RuleRef name, d) :: r -> 
                let Rule(_, cases) = find_parse_rule rules name in  
                (
                match try_cases cases input i with
                | Some( d, new_index ) -> m r new_index (d :: cap_list) env 
                | None -> None
                )

            | (ParserRef(lexer_name, parser_name), String(value, meta)) :: r ->
                let lexer = find_lexer lexer_name in
                let tokens = lex lexer value in
                let output = parse tokens parser_name in
                m r (i + 1) (output :: cap_list) env

            | (ParserRef(lexer_name, parser_name), _) :: r -> None
        in

        let sub_list = sub input index in

        match List.length sub_list < List.length ps with
        | true -> None
        | false -> let targets = zip ps sub_list in m targets index [] []

    and try_cases (cases : parser_case list) (input : data list) index : (data * int) option = 
        Some( Atom("TODO", {start_index=0; end_index=0}), 0 )

    and parse (input : data list)
              (initial_parser_name : string) : data =

        let Parser(_, rules) = find_parser initial_parser_name in 
        let Rule(_, cases) = find_parse_rule rules "main" in
        match try_cases cases input 0 with
        | Some( output, final_index ) -> Atom( "todo", {start_index = 0; end_index = 0}) (* TODO : check final_index is good and return output *)
        | None -> Atom( "todo", {start_index = 0; end_index = 0}) (* TODO : throw exception? *)

    in

    parse


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
