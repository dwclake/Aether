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

let to_string(token: t): string = {
    switch token {
        | Ident(id) => id
        | Int(x) => x

        | Fn => "fn"
        | Let => "let"
        | Bind => "bind"
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
        | COLON => ":"
        | Pipe => "|"
        | Comma => ","
        | DOT => "."
        
        | Asterisk => "*"
        | Bang => "!"
        | Question => "?"
        | Tilde => "~"
        | Pound => "#"
        | At => "@"
        | Amp => "&"
        | Dollar => "$"

        | ILLEGAL => "illegal"
        | Eof => "eof"
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
        | COLON => Some(':')
        | Pipe => Some('|')
        | Comma => Some(',')
        | DOT => Some('.')
        
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

let of_char(c: char): t = {
    switch c {
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
    | ':' => COLON
    | '|' => Pipe
    | ',' => Comma
    | '.' => DOT
    
    | '*' => Asterisk
    | '!' => Bang
    | '?' => Question
    | '~' => Tilde
    | '#' => Pound
    | '@' => At
    | '&' => Amp
    | '$' => Dollar
    
    | '\000' => Eof
    | _ => ILLEGAL
    }

}

let parse_keyword(s: string) = {
    switch s {
        | "fn" => Some(Fn)
        | "let" => Some(Let)
        | "bind" => Some(Bind)
        | "match" => Some(Match)
        | "true" => Some(True)
        | "false" => Some(False)
        | "return" => Some(Return)
        | "if" => Some(If)
        | "else" => Some(Else)
        | _ => None
    }
}
