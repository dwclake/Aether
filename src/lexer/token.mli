type t =
    | IDENT of string
    | INT of string

    | FN
    | LET
    | BIND
    | MATCH
    | TRUE
    | FALSE
    | RETURN
    | IF
    | ELSE

    | LPAREN
    | RPAREN
    | LBRACK
    | RBRACK
    | LSQUIRLY
    | RSQUIRLY

    | ASSIGN
    | SLIMARROW
    | FATARROW

    | EQUALS
    | NOTEQ

    | PLUS
    | MINUS
    | MODULO
    | CARET

    | FORSLASH
    | BACKSLASH

    | SINGLEQUOTE
    | DOUBLEQUOTE
    | BACKTICK

    | GREATER
    | LESSER

    | GREATEREQ
    | LESSEREQ

    | SEMICOLON
    | COLON
    | PIPE
    | COMMA
    | DOT

    | ASTERISK
    | BANG
    | QUESTION
    | TILDE
    | POUND
    | AT
    | AMP
    | DOLLAR

    | ILLEGAL
    | EOF
[@@deriving show, eq]

val of_char: char -> t
val to_string: t -> string
val to_char: t -> char option
val try_keyword: string -> t option
