
open Base
open Construction

exception RuleNotFoundError of int

let lex (lexer : lexer) (input : string) : data list = 
    (* TODO need to determine a non-whitespace removing option for strings etc *)
    let is_whitespace c = c = ' ' || c = '\t' || c = '\r' || c = '\n' in

    let gen_rules (Lexer (_, rules)) : (Str.regexp * constructor) list = 
        List.map (fun (Rule(rule,cons) : lexer_rule) -> (Str.regexp rule, cons)) rules
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
            | None -> raise (RuleNotFoundError !index)
    done 
    ; List.map (fun (v, c, s, e) -> construct c [String(v, {start_index = s; end_index = e - 1})] []) (List.rev !output)

