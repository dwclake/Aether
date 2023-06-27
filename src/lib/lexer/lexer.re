type t = {
    input: string,
    pos: int,
    read_pos: int,
    ch: char
};

type lex_r<'a> = {
    ..
    lexer: t
} as 'a;

let create(~input: string): t = {
    let ch = switch input {
        | "" => '\000'
        | _ => input.[0]
    };

    {   input,
        pos: 0,
        read_pos: 1,
        ch
    }
};

let advance(~count=1, lexer: t): t = {
    let readp = lexer.pos + count;
    let ch = if (readp >= String.length(lexer.input)) {
            '\000';
        } else {
            String.get(lexer.input, readp);
    };

    {   ...lexer,
        pos: readp,
        read_pos: readp + 1,
        ch,
    }
};

let peek(lexer: t): char = {
    let ch = if (lexer.read_pos >= String.length(lexer.input)) {
            '\000';
        } else {
            String.get(lexer.input, lexer.read_pos);
    };

    ch
};

let rec skip_whitespace(lexer: t): t = {
    switch lexer.ch {
        | ' ' | '\t' | '\n' | '\r' => skip_whitespace(advance(lexer))
        | _ => lexer
    }
};

let is_letter = { fun
    | 'a'..'z' | 'A'..'Z' | '_' => true
    | _ => false
};

let is_integer = { fun
    | '0'..'9' => true
    | _ => false
};

let is_number = { fun 
    | '0'..'9' | '.' => true
    | _ => false
}

let is_alphanumeric = { fun
    | ch when is_letter(ch) => true
    | ch when is_integer(ch) => true
    | _ => false
};

let rec read_sequence(~acc="", ~predicate, lexer: t): lex_r<{.. literal: string}> = {
    switch lexer.ch {
        | ch when predicate(ch) => {
            read_sequence(
                advance(lexer), 
                ~predicate,  
                ~acc=acc ++ Core.Char.to_string(ch)
            );
        }
        | _ => { as _; 
            pub lexer = lexer; 
            pub literal = acc;
        }
    }
};

let compound_or(lexer: t, ~default: Token.t, ~rules): lex_r<{.. token: Token.t}> = {
    let next_ch = peek(lexer);
    
    let rec compound_or' = { fun
        | [] => default 
        | [h,...t] => {
            let (ch, tok) = h;

            if(next_ch == ch) {
                tok
            } else {
                compound_or'(t);
            }
        }
    };
    let tok = compound_or'(rules);

    switch tok {
        | tok when tok == default => { as _; 
            pub lexer = advance(lexer); 
            pub token = tok;
        }
        | _ => { as _; 
            pub lexer = advance(lexer, ~count=2); 
            pub token = tok;
        }
    }
};

let next_token(lexer: t): lex_r<{.. token: Token.t}> = {
    let lexer = skip_whitespace(lexer);

    switch lexer.ch {
        // Idenifiers and keywords
        | ch when is_letter(ch) => {
            let lex = read_sequence(lexer, ~predicate=is_alphanumeric);
            let token = switch (Token.parse_keyword(lex#literal)) {
                | Some(token) => token
                | None => Token.Ident(lex#literal)
            };

            { as _; 
                pub lexer = lex#lexer; 
                pub token = token;
            }
        }
        // Integers
        | ch when is_integer(ch) => {
            let lex = read_sequence(lexer, ~predicate=is_number);
            let token = if (String.exists(ch => ch == '.', lex#literal)) {
                Token.Float(lex#literal)
            } else {
                Token.Int(lex#literal)
            };
            
            { as _; 
                pub lexer = lex#lexer; 
                pub token = token;
            }
        }
        // Compound operators
        | ch when ch == '>' => {
            compound_or(lexer, ~default=Token.Greater, ~rules=[('=', Token.GreaterEq)])
        }
        | ch when ch == '<' => {
            compound_or(lexer, ~default=Token.Lesser, ~rules=[('=', Token.LesserEq)])
        }
        | ch when ch == '!' => {
            compound_or(lexer, ~default=Token.Bang, ~rules=[('=', Token.NotEq)])
        }
        | ch when ch == '-' => {
            compound_or(lexer, ~default=Token.Minus, ~rules=[('>', Token.SlimArrow)])
        }
        | ch when ch == '=' => {
            compound_or(lexer, ~default=Token.Assign, ~rules=[
                ('=', Token.EqualTo),
                ('>', Token.FatArrow)
            ])
        }
        | ch when ch == '(' => {
            compound_or(lexer, ~default=Token.Lparen, ~rules=[(')', Token.Unit)])
        }
        // Individual characters
        | ch => { as _;
            pub lexer = advance(lexer); 
            pub token = Token.of_char(ch);
        }
    }
};
