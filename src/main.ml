
open Base

exception LexRuleNotFoundError of int
exception ConstructNoCapturesFoundError
exception ConstructFailedRefLookup
exception ConstructNthIndexOutOfRange of int * int 

(* todo *)
(* data_to_lexer, data_to_parser *)
let to_eval (input:data) : string = "todo"

let rec construct (c : constructor) (captures: data list) (var_env : (string * data) list) : data = 

    let l_nth l i = 
        if i >= List.length l then
            raise (ConstructNthIndexOutOfRange(i, (List.length l)))
        ;
        List.nth l i
    in

    let select_capture (ref_nums : int list) (captures : data list) : data =
            
        let rec nest rs (c : data) =
            match (rs, c) with
            | ([], _) -> c
            | (1::[], (Atom(name, _) as d)) -> d
            | (1::[], (String(value, _) as d)) -> d
            | (r::rest, Fun(_, params, _)) -> nest rest (l_nth params r)
            | (_, _) -> raise ConstructFailedRefLookup 
        in

        let first_ref = l_nth ref_nums 0 in
        
        nest (List.tl ref_nums) (l_nth captures first_ref)
    in

    if List.length captures < 1 then
        raise ConstructNoCapturesFoundError 
    ;
    
    let pull_meta : data -> meta = function Atom(_, m) -> m | Fun(_, _, m) -> m | String(_, m) -> m in

    let { start_index = start_index } = pull_meta (List.nth captures 0) in
    let { end_index = end_index } = pull_meta (List.nth captures ((List.length captures) - 1)) in
    let meta_value = {start_index = start_index ; end_index = end_index } in

    match c with
    | Atom(name) -> Atom(name, meta_value) 
    | Fun(name, params) -> Fun( name 
                              , List.map (fun param -> construct param captures var_env) params
                              , meta_value 
                              )
    | CaptureRef(ref_nums) -> select_capture ref_nums captures  
    | Var(name) -> Atom("blah", meta_value)

(* todo *)
let lex (lexer : lexer) (input : string) : data list = 
    let is_whitespace c = c = ' ' || c = '\t' || c = '\r' || c = '\n' in

    let gen_rules (Lexer (_, rules)) : (Str.regexp * constructor) list = 
        List.map (fun (Lex(rule,cons)) -> (Str.regexp rule, cons)) rules
    in

    let rules = gen_rules lexer in

    let try_rules s i = 
        let is_empty = function {contents = None} -> true | _ -> false in
        let result = ref None in 
        List.iter (fun (rule, cons) -> 
            if is_empty result && Str.string_match rule s i then
                result := Some( (Str.matched_string s, cons) )
        ) rules
        ; match !result with
        | Some(_) as res -> (res, Str.match_end())
        | None as res -> (res, i)
    in 

    let index = ref 0 in
    let output = ref [] in
    while !index < String.length input do
        if is_whitespace input.[!index] then
            index := !index + 1
        else
            let (result, new_index) = try_rules input !index in 
            match result with
            | Some( (value, cons) ) -> output := (value, cons, !index, new_index) :: !output ; index := new_index
            | None -> raise (LexRuleNotFoundError !index)
    done 
    ; List.iter (fun (x,_,_,_) -> Printf.printf "%s\n" x) (List.rev !output)
    ; List.map (fun (v, c, s, e) -> construct c [String(v, {start_index = s; end_index = e})] []) (List.rev !output)

(* find_lexer, find_parser, find_rule, match_pattern, construct *)

(* todo *)
let parse (lexers : lexer list) 
          (parsers : parser list)
          (input : data list)
          (initial_parser : string) : data =

          Atom( "todo", {start_index = 0; end_index = 0})

let l = Lexer("lexer", [Lex("[a]", Atom "blah"); Lex("[b]", Atom "blah")] )

;;



lex l "  a\tb \n"
