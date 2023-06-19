[@deriving (show, eq)]
type option_t = option(Token.t);

[@deriving (show, ord)]
type precedence = 
    [ `Lowest
    | `Equals
    | `LessGreater
    | `Sum
    | `Product
    | `Prefix
    | `Call
    | `Index
];

type t = {
    l: Lexer.t,
    current: option_t,
    peek: option_t
};

let create: Lexer.t => t
let next_token: t => t
let parse_program: t => (t, Ast.program)
