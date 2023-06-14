type t = {
    input: string,
    pos: int,
    read_pos: int,
    ch: char
};

type lex_r<'a> = {
    ..
    l: t
} as 'a;

let create = (~input: string): t => {
    let ch = switch input {
        | "" => '\000'
        | _ => input.[0]
    };

    {
        input,
        pos: 0,
        read_pos: 1,
        ch
    }
};

let advance = (~count=1, l: t): t => {
    let rp = l.pos + count;
    let ch = if(rp >= String.length(l.input)) {
        '\000';
    } else {
        String.get(l.input, rp);
    };

    {
        ...l,
        pos: rp,
        read_pos: rp + 1,
        ch,
    }
};

let peek = (l: t): char => {
    let ch = if(l.read_pos >= String.length(l.input)) {
        '\000';
    } else {
        String.get(l.input, l.read_pos);
    };

    ch
}

let rec skip_whitespace = (l: t) => {
    switch l.ch {
        | ' ' | '\t' | '\n' | '\r' => skip_whitespace(advance(l))
        | _ => l
    };
};

let is_letter = { fun
    | 'a'..'z' | 'A'..'Z' | '_' => true
    | _ => false
};

let is_number = { fun
    | '0'..'9' => true
    | _ => false
};

let is_alphanumeric = { fun
    | ch when is_letter(ch) => true
    | ch when is_number(ch) => true
    | _ => false
};

let rec read_sequence = (~s="", ~predicate, l: t): lex_r<{.. literal: string}> => {
    switch l.ch {
        | ch when predicate(ch) => {
            read_sequence(
                advance(l), 
                ~predicate,  
                ~s=s ++ Core.Char.to_string(ch)
            );
        }
        | _ => { as _; 
            pub l = l; 
            pub literal = s;
        }
    };
};

let compound_or = (l: t, ~default: Token.t, ~rules): lex_r<{.. t: Token.t}> => {
    let next_ch = peek(l);
    
    let rec loop = { fun
        | [] => default 
        | [h,...t] => {
            let (ch, tok) = h;
            if(next_ch == ch) {
                tok
            } else {
                loop(t);
            }
        }
    }

    let tok = loop(rules);

    switch tok {
        | tok when tok == default => {as _; pub l = advance(l); pub t = tok;}
        | _ => { as _; 
            pub l = advance(l, ~count=2); 
            pub t = tok;
        }
    }
}

let next_token = (l: t): lex_r<{.. t: Token.t}> => {
    let l = skip_whitespace(l);

    switch l.ch {
        // Idenifiers and keywords
        | ch when is_letter(ch) => {
            let lex = read_sequence(l, ~predicate=is_alphanumeric);
            let token = switch (Token.parse_keyword(lex#literal)) {
                | Some(t) => t
                | None => Token.IDENT(lex#literal)
            };

            { as _; 
                pub l = lex#l; 
                pub t = token;
            }
        }
        // Integers
        | ch when is_number(ch) => {
            let lex = read_sequence(l, ~predicate=is_number);
            
            { as _; 
                pub l = lex#l; 
                pub t = Token.INT(lex#literal)
            }
        }
        // Compound operators
        | ch when ch == '>' => {
            compound_or(l, ~default=Token.GREATER, ~rules=[('=', Token.GREATEREQ)])
        }
        | ch when ch == '<' => {
            compound_or(l, ~default=Token.LESSER, ~rules=[('=', Token.LESSEREQ)])
        }
        | ch when ch == '!' => {
            compound_or(l, ~default=Token.BANG, ~rules=[('=', Token.NOTEQ)])
        }
        | ch when ch == '-' => {
            compound_or(l, ~default=Token.MINUS, ~rules=[('>', Token.SLIMARROW)])
        }
        | ch when ch == '=' => {
            compound_or(l, ~default=Token.ASSIGN, ~rules=[
                ('=', Token.EQUALS),
                ('>', Token.FATARROW)
            ])
        }
        // Individual characters
        | ch => { as _; 
            pub l = advance(l); 
            pub t = Token.of_char(ch);
        }
    }
};
