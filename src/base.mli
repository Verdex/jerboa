
type meta = { start_index : int; end_index : int }

type data = 
    | Atom of string * meta
    | Fun of string * data list * meta
    | String of string * meta

type constructor =
    | Atom of string
    | Fun of string * constructor list
    | CaptureRef of int list (* %1 , %1.%2 , etc  *)
    | Var of string 

type pattern =
    | Atom of string
    | Fun of string * pattern list
    | Var of string
    | WildCard
    | RuleRef of string
    | ParserRef of string * string

type lexer_rule = Rule of string * constructor

type lexer = Lexer of string * lexer_rule list

type parser_case = Case of pattern list * constructor

type parser_rule = Rule of string * parser_case list

type parser = Parser of string * parser_rule list

