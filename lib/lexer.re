type t = {
    input: string,
    _pos: int,
    read_pos: int,
    ch: char
};

let create = (~input: string): t => {
    {
        input,
        _pos: 0,
        read_pos: 1,
        ch: input.[0]
    }
};

let advance = (l: t): t => {
    let ch = if(l.read_pos >= String.length(l.input)) {
        '\000';
    } else {
        String.get(l.input, l.read_pos);
    };

    {
        ...l,
        _pos: l.read_pos,
        read_pos: l.read_pos + 1,
        ch,
    }
};

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

let next_token = (l: t): (t, Token.t) => {
    let l = skip_whitespace(l);

    let (l, tok) = switch l.ch {

        | ch when is_letter(ch) => {
            let (l, literal) = read_sequence(l, ~predicate=is_alphanumeric);
            let token = switch (Token.parse_keyword(literal)) {
                | Some(t) => t
                | None => Token.IDENT(literal)
            };
            (l, token);
        }

        | ch when is_number(ch) => {
            let (l, literal) = read_sequence(l, ~predicate=is_number);
            (l, Token.INT(literal));
        }

        | ch when ch == '>' => {
            let l = advance(l);
            switch l.ch {
                | '=' => (advance(l), Token.GREATER_EQ)
                | _ => (l, Token.GREATER)
            }
        }

        | ch when ch == '<' => {
            let l = advance(l);
            switch l.ch {
                | '=' => (advance(l), Token.LESSER_EQ)
                | _ => (l, Token.LESSER)
            }
        }

        | ch when ch == '!' => {
            let l = advance(l);
            switch l.ch {
                | '=' => (advance(l), Token.NOT_EQUALS)
                | _ => (l, Token.BANG)
            }
        }

        | ch when ch == '=' => {
            let l = advance(l);
            switch l.ch {
                | '=' => (advance(l), Token.EQUALS)
                | '>' => (advance(l), Token.FAT_ARROW)
                | _ => (l, Token.ASSIGN)
            }
        }

        | ch when ch == '-' => {
            let l = advance(l);
            switch l.ch {
                | '>' => (advance(l), Token.SLIM_ARROW)
                | _ => (l, Token.MINUS)
            }
        }

        | ch => (advance(l), Token.of_char(ch));
    };
    
    (l, tok);
};
