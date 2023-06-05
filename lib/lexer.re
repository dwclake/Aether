type t = {
    input: string,
    read_pos: int,
    ch: char
};

let create = (~input: string): t => {
    {
        input,
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
        read_pos: l.read_pos + 1,
        ch,
    }
};

let next_token = (l: t): (t, Token.t) => {
    let tok = switch l.ch {
        | ch => Token.of_char(ch)
    };
    
    (read_char(l), tok);
};
