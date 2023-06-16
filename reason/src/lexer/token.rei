[@deriving show]
type t =
    | IDENT(string)
    | INT(string)

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
    | EOF;

let of_char: char => t;
let to_string: t => string;
let to_char: t => option(char);
let parse_keyword: string => option(t);
