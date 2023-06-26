[@deriving (show{with_path: false}, eq)]
type t =
    | Ident(string)
    | Int(string)
    | Float(string)

    | Fn
    | Let
    | Const
    | Mut
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
    | Colon
    | Pipe
    | Comma
    | Dot

    | Asterisk
    | Bang
    | Question
    | Tilde
    | Pound
    | At
    | Amp
    | Dollar

    | Unit
    | Illegal
    | Eof;

let of_char: char => t;
let to_string: t => string;
let to_string_opt: option(t) => string;
let to_char: t => option(char);
let parse_keyword: string => option(t);
