type t = 
    (* Literals *)
    | Ident of string
    | Int of string
    | Float of string
    (* Keywords *)
    | Fn
    | Let
    | Const
    | Mut
    | Rec
    | Match
    | True
    | False
    | Return
    | If
    | Else
    (* Operators *)
    | Assign
    | Arrow
    | FatArrow
    | Plus
    | Minus
    | Percent
    | Caret
    | Slash
    | Backslash
    | Pipe
    | Asterisk
    | Bang
    | Question
    | Tilde
    | Pound
    | Address
    | Ampersand
    | Dollar
    (* Punctuation *) 
    | Lparen
    | Rparen
    | Lbracket
    | Rbracket
    | Lbrace
    | Rbrace
    | Semicolon
    | Colon
    | Comma
    | Dot
    | Quote
    | DoubleQuote
    | Backtick
    (* Comparators *)
    | Eq
    | Neq
    | Gt
    | Lt
    | Geq
    | Leq
    (* Logical *)
    | And
    | Or
    (* Misc *)
    | Illegal
    | Eof
    [@@deriving show {with_path=false}, eq]

val of_char: char -> t
val to_string: t -> string
val to_string_opt: t option -> string
val try_keyword: string -> t option
