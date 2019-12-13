
lex jerboa {
    "[\n\r\t ]+" => nothing;
    "=>" => rarrow;
    ";" => semicolon;
    "," => comma;
    ":" => colon;
    "|" => or;
    "_" => wild;
    "{" => lcurl;
    "}" => rcurl;
    "%(" => lparen;
    "%)" => rparen;
    "%l[%w_]*" => symbol(_);
    "%u[%w_]*" => variable(_);
    "%$%l[%w_]*" => rule_reference(_);
    "%%%d+" => capture_reference(_);
    "'.-'" => string(_);
    '".-"' => string(_);
}

