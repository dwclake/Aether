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

class t: lexer:Lexer.t ref -> 
    object
        val lexer': Lexer.t ref
        method lexer: Lexer.t ref
        
        val mutable current': option_t
        method current: option_t 
        method set_current: option_t -> unit
        
        val mutable peek': option_t
        method peek: option_t 
        method set_peek: option_t -> unit
end

val next_token: ?count:int -> t ref -> unit
val parse_program: t ref -> Ast.program
