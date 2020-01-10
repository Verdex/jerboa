
open Base
open Construction
open Util
open Lexer
open Debug (* todo *)


(* todo *)
(* data_to_lexer, data_to_parser *)
let to_eval (input:data) : string = "todo"


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
