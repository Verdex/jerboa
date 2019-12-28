
open Base

let display_meta (m : meta) = 
    let {start_index = s; end_index = e} = m in
    Printf.sprintf "{start_index = %d ; end_index = %d}" s e

let print_meta (m : meta) = Printf.printf "%s\n" (display_meta m)

let rec display_data (d : data) = 
    match d with 
    | Atom(n, m) -> Printf.sprintf "%s meta: %s" n (display_meta m) 
    | String(v, m) -> Printf.sprintf "\"%s\" meta: %s" v (display_meta m)
    | Fun(n, ps, m) -> Printf.sprintf "%s(%s) meta: %s" 
                                      n 
                                      (String.concat ",\n\t" (List.map display_data ps)) 
                                      (display_meta m)

let print_data (d : data) = Printf.printf "%s\n" (display_data d)

