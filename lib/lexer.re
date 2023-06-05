type t = {
    input: string,
    pos: int,
    read_pos: int,
    ch: char
};

let create = (~input: string): t => {
    {
        input,
        pos: 0,
        read_pos: 0,
        ch: ' '
    }
};

let update = (~l: t, p: option(int), rp: option(int), c: option(char)): t => {
    switch (p, rp, c) {
        | (Some(pos), Some(read_pos), Some(ch)) => {
            ...l,
            pos,
            read_pos,
            ch
        }
        | (Some(pos), Some(read_pos), None) => {
            ...l,
            pos,
            read_pos
        }
        | (Some(pos), None, None) => {
            ...l,
            pos
        }
        | (Some(pos), None, Some(ch)) => {
            ...l,
            pos,
            ch
        }
        | (None, None, Some(ch)) => {
            ...l,
            ch
        }
        | (None, Some(read_pos), Some(ch)) => {
            ...l,
            read_pos,
            ch
        }
        | (None, Some(read_pos), None) => {
            ...l,
            read_pos
        }
        | _ => l
    }
}

let read_char = (l: t): t => {
    let ch = if(l.read_pos >= String.length(l.input)) {
        ' ';
    } else {
        String.get(l.input, l.read_pos);
    };

    {
        ...l,
        pos: l.read_pos,
        read_pos: l.read_pos + 1,
        ch,
    }
}
