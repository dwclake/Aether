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

let is_letter = { fun
    | 'a'..'z' | 'A'..'Z' | '_' => true
    | _ => false
    };

let rec read_ident = (~s="", l: t): (t, string) => {
    switch(l.ch) {
        | ch when is_letter(ch) => {
            read_ident(read_char(l), ~s=s ++ Core.Char.to_string(ch));
        }
        | _ => (l, s);
    };
}

let next_token = (l: t): (t, Token.t) => {
    let (l, tok) = switch l.ch {
        | ch when is_letter(ch) => {
            let (l, literal) = read_ident(l);
            let tok = Token.keyword(literal);
            (l, tok);
        }
        | ch => (l, Token.of_char(ch));
    };
    
    (read_char(l), tok);
};


