
open Base
open Util

exception NoCapturesFoundError
exception FailedRefLookupError
exception VariableLookupError of string

let var_lookup (env : (string * data) list) (var_name : string) : data =
    try let (_, data) = List.find (fun (name, _) -> name = var_name) env in data
    with _ -> raise (VariableLookupError var_name)

let select_capture (ref_nums : int list) (captures : data list) : data =
        
    let rec nest rs (c : data) =
        match (rs, c) with
        | ([], _) -> c
        | (0::[], (Atom(_, _) as d)) -> d
        | (0::[], (String(_, _) as d)) -> d
        | (r::rest, Fun(_, params, _)) -> nest rest (l_nth params r)
        | (_, _) -> raise FailedRefLookupError
    in

    let first_ref = l_nth ref_nums 0 in
    
    match (List.tl ref_nums, l_nth captures first_ref) with
    | ([], (Atom(_, _) as d)) -> d
    | ([], (String(_, _) as d)) -> d
    | ([], (Fun(_, _, _) as d)) -> d
    | (refs, Fun(_, params, _)) -> nest (List.tl refs) (l_nth params (List.hd refs))
    | _ -> raise FailedRefLookupError

let rec construct (c : constructor) (captures: data list) (var_env : (string * data) list) : data = 

    if List.length captures < 1 then
        raise NoCapturesFoundError 
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
    | Var(name) -> var_lookup var_env name 

