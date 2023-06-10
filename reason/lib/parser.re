type t = {
    l: Lexer.t,

    cur_t: Token.t,
    peek_t: Token.t
}

let next_token = (p: t): t => {
    let (l, tok) = Lexer.next_token(p.l);
    {
        l,
        cur_t: p.peek_t,
        peek_t: tok
    }
}

let create = (l: Lexer.t): t => {
    {
        l,
        cur_t: Token.EOF,
        peek_t: Token.EOF
    } |> next_token |> next_token
} 
