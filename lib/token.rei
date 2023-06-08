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

    | L_PAREN
    | R_PAREN
    | L_BRACKET
    | R_BRACKET
    | L_SQUIRELY
    | R_SQUIRELY

    | ASSIGN
    | SLIM_ARROW
    | FAT_ARROW

    | EQUALS
    | NOT_EQUALS

    | PLUS
    | MINUS
    | MODULO
    | CARET

    | FORWARD_SLASH
    | BACK_SLASH

    | SINGLE_QUOTE
    | DOUBLE_QUOTE
    | BACK_TICK

    | GREATER
    | LESSER

    | GREATER_EQ
    | LESSER_EQ

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
