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

let read_char = (l: t): t => {
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
    | ' ' | '\t' | '\n' | '\r' => skip_whitespace(read_char(l))
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
}

let rec read_sequence = (~s="", ~predicate, l: t): (t, string) => {
    switch l.ch {
        | ch when predicate(ch) => {
            read_sequence(
                read_char(l), 
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
        | ch => (read_char(l), Token.of_char(ch));
    };
    
    (l, tok);
};
