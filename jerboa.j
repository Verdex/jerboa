
lex jerboa {
    "[\n\r\t ]+" => nothing;
    "=>" => rarrow;
    "!" => nothing;
    ";" => semicolon;
    "," => comma;
    ":" => colon;
    "|" => or;
    "_" => wild;
    "{" => lcurl;
    "}" => rcurl;
    "%(" => lparen;
    "%)" => rparen;
    "%l[%w_]*" => symbol(%1);
    "%u[%w_]*" => variable(%1);
    "%$%l[%w_]*" => rule_reference(%1);
    "%%%d+" => capture_reference(%1);
    "'.-'" => string(%1);
    '".-"' => string(%1);
}

parse jerboa {
    main: $definitions => main(%1);
    
    definitions: 
        $definition $definitions => defs(%1, %2)
        | !  => null
        ;

    definition: 
        $lex_definition => lex(%1)
        | $parse_definition => parse(%1)
        ;

    lex_definition:
        symbol("lex") symbol(Name) lcurl %lex_rules rcurl 
        => lex_rules( Name, %4 ) 
        ;
}
