type option_t = Token.t option
[@@deriving show, eq];;

type precedence = 
    [ `Lowest
    | `Equals
    | `LessGreater
    | `Sum
    | `Product
    | `Prefix
    | `Call
    | `Index
][@@deriving ord]

type t = { lexer: Lexer.t ref
         ; current: option_t
         ; peek: option_t
         }

val create: lexer:Lexer.t ref -> t
val next_token: ?count:int -> t -> t
val parse_program: t -> t * Ast.program
