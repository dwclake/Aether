[@deriving (show, eq)]
type t =
    | Ident(string)
    | Int(string)

    | Fn
    | Let
    | Bind
    | Match
    | True
    | False
    | Return
    | If
    | Else

    | Lparen
    | Rparen
    | Lbracket
    | Rbracket
    | Lsquirly
    | Rsquirly

    | Assign
    | SlimArrow
    | FatArrow

    | EqualTo
    | NotEq

    | Plus
    | Minus
    | Modulo
    | Caret

    | Forwardslash
    | Backslash

    | SingleQuote
    | DoubleQuote
    | Backtick

    | Greater
    | Lesser

    | GreaterEq
    | LesserEq

    | Semicolon
    | COLON
    | Pipe
    | Comma
    | DOT

    | Asterisk
    | Bang
    | Question
    | Tilde
    | Pound
    | At
    | Amp
    | Dollar

    | ILLEGAL
    | Eof;

let of_char: char => t;
let to_string: t => string;
let to_char: t => option(char);
let parse_keyword: string => option(t);