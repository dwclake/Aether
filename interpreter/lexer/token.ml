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

let to_string = function
    | Ident id -> id
    | Int x -> x
    | Float x -> x

    | Fn -> "fn"
    | Let -> "let"
    | Const -> "const"
    | Mut -> "mut"
    | Match -> "match"
    | Rec -> "rec"
    | True -> "true"
    | False -> "false"
    | Return -> "return"
    | If -> "if"
    | Else -> "else"

    | Assign -> "="
    | FatArrow -> "=>"
    | Arrow -> "->"
    | Plus -> "+"
    | Minus -> "-"
    | Percent -> "%"
    | Caret -> "^"
    | Slash -> "/"
    | Backslash -> "\\"
    | Asterisk -> "*"
    | Bang -> "!"
    | Question -> "?"
    | Tilde -> "~"
    | Pound -> "#"
    | Address -> "@"
    | Ampersand -> "&"
    | Dollar -> "$"

    | Lparen -> "("
    | Rparen -> ")"
    | Lbracket -> "["
    | Rbracket -> "]"
    | Lbrace -> "{"
    | Rbrace -> "}"
    | Semicolon -> ";"
    | Colon -> ":"
    | Pipe -> "|"
    | Comma -> ","
    | Dot -> "."
    | Quote -> "'"
    | DoubleQuote -> "\""
    | Backtick -> "`"

    | Eq -> "=="
    | Neq -> "!="
    | Gt -> ">"
    | Lt -> "<"
    | Geq -> ">="
    | Leq -> "<="

    | And -> "&&"
    | Or -> "||"

    | Illegal -> "illegal"
    | Eof -> "eof"
;;

let to_string_opt = function
    | Some Ident id -> id
    | Some Int x -> x
    | Some Float x -> x

    | Some Fn -> "fn"
    | Some Let -> "let"
    | Some Const -> "const"
    | Some Mut -> "mut"
    | Some Match -> "match"
    | Some Rec -> "rec"
    | Some True -> "true"
    | Some False -> "false"
    | Some Return -> "return"
    | Some If -> "if"
    | Some Else -> "else"

    | Some Assign -> "="
    | Some FatArrow -> "=>"
    | Some Arrow -> "->"
    | Some Plus -> "+"
    | Some Minus -> "-"
    | Some Percent -> "%"
    | Some Caret -> "^"
    | Some Slash -> "/"
    | Some Backslash -> "\\"
    | Some Asterisk -> "*"
    | Some Bang -> "!"
    | Some Question -> "?"
    | Some Tilde -> "~"
    | Some Pound -> "#"
    | Some Address -> "@"
    | Some Ampersand -> "&"
    | Some Dollar -> "$"

    | Some Lparen -> "("
    | Some Rparen -> ")"
    | Some Lbracket -> "["
    | Some Rbracket -> "]"
    | Some Lbrace -> "{"
    | Some Rbrace -> "}"
    | Some Semicolon -> ";"
    | Some Colon -> ":"
    | Some Pipe -> "|"
    | Some Comma -> ","
    | Some Dot -> "."
    | Some Quote -> "'"
    | Some DoubleQuote -> "\""
    | Some Backtick -> "`"

    | Some Eq -> "=="
    | Some Neq -> "!="
    | Some Gt -> ">"
    | Some Lt -> "<"
    | Some Geq -> ">="
    | Some Leq -> "<="

    | Some And -> "&&"
    | Some Or -> "||"

    | Some Illegal -> "illegal"
    | Some Eof -> "eof"
    | None -> "illegal"
;;

let of_char = function
    | '(' -> Lparen
    | ')' -> Rparen
    | '[' -> Lbracket
    | ']' -> Rbracket
    | '{' -> Lbrace
    | '}' -> Rbrace

    | '=' -> Assign
    | '+' -> Plus
    | '-' -> Minus
    | '%' -> Percent
    | '^' -> Caret
    | '*' -> Asterisk
    | '!' -> Bang
    | '?' -> Question
    | '~' -> Tilde
    | '#' -> Pound
    | '@' -> Address
    | '&' -> Ampersand
    | '$' -> Dollar
    | '/' -> Slash
    | '\\' -> Backslash

    | ';' -> Semicolon
    | ':' -> Colon
    | '|' -> Pipe
    | ',' -> Comma
    | '.' -> Dot
    | '\'' -> Quote
    | '\"' -> DoubleQuote
    | '`' -> Backtick

    | '>' -> Gt
    | '<' -> Lt
    
    | '\000' -> Eof
    | _ -> Illegal
;;

let try_keyword = function
    | "fn" -> Some Fn
    | "let" -> Some Let
    | "const" -> Some Const
    | "mut" -> Some Mut
    | "match" -> Some Match 
    | "rec" -> Some Rec
    | "true" -> Some True
    | "false" -> Some False
    | "return" -> Some Return
    | "if" -> Some If
    | "else" -> Some Else
    | _ -> None
;;
