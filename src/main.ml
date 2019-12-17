
open Base

(* todo *)
(* data_to_lexer, data_to_parser *)
let to_eval (input:data) : string = "todo"

(* todo *)
let lex (l : lexer) (input : string) : data = Atom("todo", {start_index = 0; end_index = 0})
(* automatically prune whitespace  *)

(* find_lexer, find_parser, find_rule, match_pattern, construct *)

(* todo *)
let parse (lexers : lexer list) 
          (parsers : parser list)
          (input : data list)
          (initial_parser : string) : data =

          Atom( "todo", {start_index = 0; end_index = 0})
