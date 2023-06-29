[@deriving (show, eq)]
type option_t = option(Token.t);

type t = {
    lexer: ref(Lexer.t),
    current: option_t,
    peek: option_t
};

let create: ref(Lexer.t) => t
let next_token: t => t
let parse_program: t => (t, Ast.program)
