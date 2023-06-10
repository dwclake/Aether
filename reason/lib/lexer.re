type t = {
    input: string,
    pos: int,
    read_pos: int,
    ch: char
};

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
    }

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

let rec read_sequence = (~s="", ~predicate, l: t): (t, string) => {
    switch l.ch {
        | ch when predicate(ch) => {
            read_sequence(
                advance(l), 
                ~predicate,  
                ~s=s ++ Core.Char.to_string(ch)
            );
        }
        | _ => (l, s);
    };
};

let compound_or = (l: t, ~default: Token.t, ~rules): (t, Token.t) => {
    let next_ch = peek(l);
    
    let rec loop = { fun
        | [] => default 
        | [h, ...t] => {
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
        | tok when tok == default => (advance(l), tok)
        | _ => (advance(l, ~count=2), tok)
    }
}

let next_token = (l: t): (t, Token.t) => {
    let l = skip_whitespace(l);

    let (l, tok) = switch l.ch {
        // Idenifiers and keywords
        | ch when is_letter(ch) => {
            let (l, literal) = read_sequence(l, ~predicate=is_alphanumeric);
            let token = switch (Token.parse_keyword(literal)) {
                | Some(t) => t
                | None => Token.IDENT(literal)
            };
            (l, token);
        }
        // Integers
        | ch when is_number(ch) => {
            let (l, literal) = read_sequence(l, ~predicate=is_number);
            (l, Token.INT(literal));
        }
        // Compound operators
        | ch when ch == '>' => {
            compound_or(l, ~default=Token.GREATER, ~rules=[('=', Token.GREATER_EQ)])
        }
        | ch when ch == '<' => {
            compound_or(l, ~default=Token.LESSER, ~rules=[('=', Token.LESSER_EQ)])
        }
        | ch when ch == '!' => {
            compound_or(l, ~default=Token.BANG, ~rules=[('=', Token.NOT_EQUALS)])
        }
        | ch when ch == '-' => {
            compound_or(l, ~default=Token.MINUS, ~rules=[('>', Token.SLIM_ARROW)])
        }
        | ch when ch == '=' => {
            compound_or(l, ~default=Token.ASSIGN, ~rules=[
                ('=', Token.EQUALS),
                ('>', Token.FAT_ARROW)
            ])
        }
        // Individual characters
        | ch => (advance(l), Token.of_char(ch));
    };
    
    (l, tok);
};
