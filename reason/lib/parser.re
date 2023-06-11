type t = {
    l: Lexer.t,

    cur_t: Token.t,
    peek_t: Token.t
}

let next_token = (p: t): t => {
    let lex = Lexer.next_token(p.l);
    {
        l: lex#l,
        cur_t: p.peek_t,
        peek_t: lex#t
    }
}

let create = (l: Lexer.t): t => {
    {
        l,
        cur_t: Token.EOF,
        peek_t: Token.EOF
    } 
    |> next_token |> next_token
} 
