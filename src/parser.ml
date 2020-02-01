
open Base
open Construction
open Util
open Lexer

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
                          (index : int) : (data list * (string * data) list) option * int =

        let rec m (ts : (pattern * data) list) 
                  (i : int)
                  (cap_list : data list)
                  (env : (string * data) list) : (data list * (string * data) list) option * int =
            match ts with
            | [] -> (Some(List.rev cap_list, env), i)

            | (Atom(p_name), (Atom(d_name, _) as d)) :: r when p_name = d_name 
                -> m r (i + 1) (d :: cap_list) env

            | (Atom(_), _) :: _ -> (None, i) 

            | (Fun(p_name, p_params), Fun(d_name, d_params, meta)) :: r when p_name = d_name
                -> ( 
                   match match_pattern rules p_params d_params 0 with
                   | (Some(sub_cap, sub_env), final_index) when final_index = List.length d_params
                     -> m r (i + 1) (Fun(d_name, sub_cap, meta) :: cap_list) (env_merge sub_env env)
                   | _ -> (None, i)
                   )

            | (Fun(_, _), _) :: _ -> (None, i)

            | (WildCard, d) :: r -> m r (i + 1) (d :: cap_list) env 

            | (Var name, d) :: r -> m r (i + 1) (d :: cap_list) (env_merge [(name, d)] env)

            | (RuleRef name, d) :: r -> 
                let Rule(_, cases) = find_parse_rule rules name in  
                (
                match try_cases rules cases input i with
                | Success( Some d, new_index ) -> m r new_index (d :: cap_list) env 
                | Success( None, new_index ) -> m r new_index cap_list env
                | Fail new_index -> (None, new_index)
                )

            | (ParserRef(lexer_name, parser_name), String(value, meta)) :: r ->
                let lexer = find_lexer lexer_name in
                let tokens = lex lexer value in
                (
                match parse tokens parser_name with
                | Success(Some data, final_index) when (final_index + 1) = String.length value 
                    -> m r (i + 1) (data :: cap_list) env
                | Success(None, final_index) when (final_index + 1) = String.length value 
                    -> m r (i + 1) cap_list env
                | _ -> (None, i)
                )

            | (ParserRef(lexer_name, parser_name), _) :: r -> (None, i)
        in

        let sub_list = sub input index in

        match List.length sub_list < List.length ps with
        | true -> (None, index)
        | false -> let targets = zip ps sub_list in m targets index [] []

    and try_cases (rules : parser_rule list) 
                  (cases : parser_case list) 
                  (input : data list) 
                  (index : int ) : parse_result = 

        let rec h cs i = 
            match cs with
            | [] -> Fail i
            | Case(pattern, constructor) :: r -> 
                (
                match match_pattern rules pattern input i with
                | (None, _) -> h r i 
                | (Some(cap_list, env), new_index) -> 
                    Success( construct constructor cap_list env, new_index )
                )
        in
        h cases index

    and parse (input : data list)
              (initial_parser_name : string) : parse_result =

        let Parser(_, rules) = find_parser initial_parser_name in 
        let Rule(_, cases) = find_parse_rule rules "main" in
        try_cases rules cases input 0 
    in

    parse

let parse (lexers : lexer list) 
          (parsers : parser list)
          (input : data list)
          (initial_parser_name : string) : parse_result =

    let parse_helper = gen lexers parsers in
    parse_helper input initial_parser_name

