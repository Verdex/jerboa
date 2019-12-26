
open Base

exception LexError of int

(* todo *)
(* data_to_lexer, data_to_parser *)
let to_eval (input:data) : string = "todo"

let rec construct (c : constructor) (captures: data list) (var_env : (string * data) list) : data = 
(* first of first capture and last of last caputure for start and end index *)
    match c with
    | Atom(name) -> Atom(name, {start_index = 0; end_index = 0} ) 
    | Fun(name, params) -> Fun( name 
                              , List.map (fun param -> construct param captures var_env) params
                              , {start_index = 0; end_index = 0} 
                              )
    | CaptureRef(ref_nums) -> Atom("blah", {start_index = 0; end_index = 0}) 
    | Var(name) -> Atom("blah", {start_index = 0; end_index = 0})

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
            | None -> raise (LexError !index)
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
