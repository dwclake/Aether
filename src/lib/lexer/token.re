[@deriving (show{with_path: false}, eq)]
type t = 
    | Ident(string)
    | Int(string)
    | Float(string)

    | Fn
    | FnAnon
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

let to_string(token: t): string = {
    switch token {
        | Ident(id) => id
        | Int(x) => x
        | Float(x) => x

        | Fn => "fn"
        | FnAnon => "%{"
        | Let => "let"
        | Const => "const"
        | Mut => "mut"
        | Match => "match"
        | True => "true"
        | False => "false"
        | Return => "return"
        | If => "if"
        | Else => "else"

        | Lparen => "("
        | Rparen => ")"
        | Lbracket => "["
        | Rbracket => "]"
        | Lsquirly => "{"
        | Rsquirly => "}"

        | Assign => "="
        | FatArrow => "=>"
        | SlimArrow => "->"

        | EqualTo => "=="
        | NotEq => "!="

        | Plus => "+"
        | Minus => "-"
        | Modulo => "%"
        | Caret => "^"

        | Forwardslash => "/"
        | Backslash => "\\"

        | SingleQuote => "'"
        | DoubleQuote => "\""
        | Backtick => "`"

        | Greater => ">"
        | Lesser => "<"
        
        | GreaterEq => ">="
        | LesserEq => "<="

        | Semicolon => ";"
        | Colon => ":"
        | Pipe => "|"
        | Comma => ","
        | Dot => "."
        
        | Asterisk => "*"
        | Bang => "!"
        | Question => "?"
        | Tilde => "~"
        | Pound => "#"
        | At => "@"
        | Amp => "&"
        | Dollar => "$"

        | Unit => "()"
        | Illegal => "illegal"
        | Eof => "eof"
    }
}

let to_string_opt(token: option(t)): string = {
    switch token {
        | Some(Ident(id)) => id
        | Some(Int(x)) => x
        | Some(Float(x)) => x

        | Some(Fn) => "fn"
        | Some(FnAnon) => "%{"
        | Some(Let) => "let"
        | Some(Const) => "const"
        | Some(Mut) => "mut"
        | Some(Match) => "match"
        | Some(True) => "true"
        | Some(False) => "false"
        | Some(Return) => "return"
        | Some(If) => "if"
        | Some(Else) => "else"

        | Some(Lparen) => "("
        | Some(Rparen) => ")"
        | Some(Lbracket) => "["
        | Some(Rbracket) => "]"
        | Some(Lsquirly) => "{"
        | Some(Rsquirly) => "}"

        | Some(Assign) => "="
        | Some(FatArrow) => "=>"
        | Some(SlimArrow) => "->"

        | Some(EqualTo) => "=="
        | Some(NotEq) => "!="

        | Some(Plus) => "+"
        | Some(Minus) => "-"
        | Some(Modulo) => "%"
        | Some(Caret) => "^"

        | Some(Forwardslash) => "/"
        | Some(Backslash) => "\\"

        | Some(SingleQuote) => "'"
        | Some(DoubleQuote) => "\""
        | Some(Backtick) => "`"

        | Some(Greater) => ">"
        | Some(Lesser) => "<"
        
        | Some(GreaterEq) => ">="
        | Some(LesserEq) => "<="

        | Some(Semicolon) => ";"
        | Some(Colon) => ":"
        | Some(Pipe) => "|"
        | Some(Comma) => ","
        | Some(Dot) => "."
        
        | Some(Asterisk) => "*"
        | Some(Bang) => "!"
        | Some(Question) => "?"
        | Some(Tilde) => "~"
        | Some(Pound) => "#"
        | Some(At) => "@"
        | Some(Amp) => "&"
        | Some(Dollar) => "$"

        | Some(Unit) => "()"
        | Some(Illegal) => "illegal"
        | Some(Eof) => "eof"
        | None => "missing token"
    }
}

let to_char(token: t): option(char) = {
    switch token {
        | Lparen => Some('(')
        | Rparen => Some(')')
        | Lbracket => Some('[')
        | Rbracket => Some(']')
        | Lsquirly => Some('{')
        | Rsquirly => Some('}')

        | Assign => Some('=')

        | Plus => Some('+')
        | Minus => Some('-')
        | Modulo => Some('%')
        | Caret => Some('^')

        | Forwardslash => Some('/')
        | Backslash => Some('\\')

        | SingleQuote => Some('\'')
        | DoubleQuote => Some('\"')
        | Backtick => Some('`')

        | Greater => Some('>')
        | Lesser => Some('<')
        
        | Semicolon => Some(';')
        | Colon => Some(':')
        | Pipe => Some('|')
        | Comma => Some(',')
        | Dot => Some('.')
        
        | Asterisk => Some('*')
        | Bang => Some('!')
        | Question => Some('?')
        | Tilde => Some('~')
        | Pound => Some('#')
        | At => Some('@')
        | Amp => Some('&')
        | Dollar => Some('$')
        
        | Eof => Some('\000')
        | _ => None
    }
}

let of_char(ch: char): t = {
    switch ch {
    | '(' => Lparen
    | ')' => Rparen
    | '[' => Lbracket
    | ']' => Rbracket
    | '{' => Lsquirly
    | '}' => Rsquirly

    | '=' => Assign

    | '+' => Plus
    | '-' => Minus
    | '%' => Modulo
    | '^' => Caret

    | '/' => Forwardslash
    | '\\' => Backslash

    | '\'' => SingleQuote
    | '\"' => DoubleQuote
    | '`' => Backtick

    | '>' => Greater
    | '<' => Lesser
    
    | ';' => Semicolon
    | ':' => Colon
    | '|' => Pipe
    | ',' => Comma
    | '.' => Dot
    
    | '*' => Asterisk
    | '!' => Bang
    | '?' => Question
    | '~' => Tilde
    | '#' => Pound
    | '@' => At
    | '&' => Amp
    | '$' => Dollar
    
    | '\000' => Eof
    | _ => Illegal
    }

}

let parse_keyword(literal: string) = {
    switch literal {
        | "fn" => Some(Fn)
        | "let" => Some(Let)
        | "const" => Some(Const)
        | "mut" => Some(Mut)
        | "match" => Some(Match)
        | "true" => Some(True)
        | "false" => Some(False)
        | "return" => Some(Return)
        | "if" => Some(If)
        | "else" => Some(Else)
        | _ => None
    }
}
