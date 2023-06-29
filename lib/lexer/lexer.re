type t = {
    input: string,
    mutable pos: int,
    mutable read_pos: int,
    mutable ch: char
};

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

let advance(~count=1, lexer: ref(t)): unit = {
    let readp = lexer^.pos + count;
    let ch = if (readp >= String.length(lexer^.input)) {
            '\000';
        } else {
            String.get(lexer^.input, readp);
    };


        lexer^.pos = readp;
        lexer^.read_pos = readp + 1;
        lexer^.ch = ch;
};

let peek(lexer: ref(t)): char = {
    if (lexer^.read_pos >= String.length(lexer^.input)) {
        '\000';
    } else {
         String.get(lexer^.input, lexer^.read_pos);
    }
};

let rec skip_whitespace(lexer: ref(t)): unit = {
    switch lexer^.ch {
        | ' ' | '\t' | '\n' | '\r' => {
            advance(lexer);
            skip_whitespace(lexer)
        }
        | _ => ()
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

let rec read_sequence(~acc="", ~predicate, lexer: ref(t)): string = {
    switch lexer^.ch {
        | ch when predicate(ch) => {
            advance(lexer);
            read_sequence(
                lexer, 
                ~predicate,  
                ~acc=acc ++ Core.Char.to_string(ch)
            );
        }
        | _ => acc
    }
};

let compound_or(lexer: ref(t), ~default: Token.t, ~rules): Token.t = {
    let next_ch = peek(lexer);
    
    let rec compound_or' = { fun
        | [] => default 
        | [h,...t] => {
            let (ch, token) = h;

            if(next_ch == ch) {
                token
            } else {
                compound_or'(t);
            }
        }
    };
    let token = compound_or'(rules);

    switch token {
        | token when token == default => { 
            advance(lexer); 
            token
        }
        | _ => {
            advance(lexer, ~count=2); 
            token
        }
    }
};

let next_token(lexer: ref(t)): Token.t = {
    skip_whitespace(lexer);

    switch lexer^.ch {
        // Idenifiers and keywords
        | ch when is_letter(ch) => {
            let literal = read_sequence(lexer, ~predicate=is_alphanumeric);
            switch (Token.parse_keyword(literal)) {
                | Some(token) => token
                | None => Token.Ident(literal)
            }
        }
        // Integers
        | ch when is_integer(ch) => {
            let literal = read_sequence(lexer, ~predicate=is_number);
            if (String.exists(ch => ch == '.', literal)) {
                Token.Float(literal)
            } else {
                Token.Int(literal)
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
        // Individual characters
        | ch => {
            advance(lexer); 
            Token.of_char(ch)
        }
    }
};
