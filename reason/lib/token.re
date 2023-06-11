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

let to_string = (token: t): string => {
    switch token {
        | IDENT(id) => id
        | INT(x) => x

        | FN => "fn"
        | LET => "let"
        | BIND => "bind"
        | MATCH => "match"
        | TRUE => "true"
        | FALSE => "false"
        | RETURN => "return"
        | IF => "if"
        | ELSE => "else"

        | LPAREN => "("
        | RPAREN => ")"
        | LBRACK => "["
        | RBRACK => "]"
        | LSQUIRLY => "{"
        | RSQUIRLY => "}"

        | ASSIGN => "="
        | FATARROW => "=>"
        | SLIMARROW => "->"

        | EQUALS => "=="
        | NOTEQ => "!="

        | PLUS => "+"
        | MINUS => "-"
        | MODULO => "%"
        | CARET => "^"

        | FORSLASH => "/"
        | BACKSLASH => "\\"

        | SINGLEQUOTE => "'"
        | DOUBLEQUOTE => "\""
        | BACKTICK => "`"

        | GREATER => ">"
        | LESSER => "<"
        
        | GREATEREQ => ">="
        | LESSEREQ => "<="

        | SEMICOLON => ";"
        | COLON => ":"
        | PIPE => "|"
        | COMMA => ","
        | DOT => "."
        
        | ASTERISK => "*"
        | BANG => "!"
        | QUESTION => "?"
        | TILDE => "~"
        | POUND => "#"
        | AT => "@"
        | AMP => "&"
        | DOLLAR => "$"

        | ILLEGAL => "illegal"
        | EOF => "eof"
    }
}

let to_char = (token: t): option(char) => {
    switch token {
        | LPAREN => Some('(')
        | RPAREN => Some(')')
        | LBRACK => Some('[')
        | RBRACK => Some(']')
        | LSQUIRLY => Some('{')
        | RSQUIRLY => Some('}')

        | ASSIGN => Some('=')

        | PLUS => Some('+')
        | MINUS => Some('-')
        | MODULO => Some('%')
        | CARET => Some('^')

        | FORSLASH => Some('/')
        | BACKSLASH => Some('\\')

        | SINGLEQUOTE => Some('\'')
        | DOUBLEQUOTE => Some('\"')
        | BACKTICK => Some('`')

        | GREATER => Some('>')
        | LESSER => Some('<')
        
        | SEMICOLON => Some(';')
        | COLON => Some(':')
        | PIPE => Some('|')
        | COMMA => Some(',')
        | DOT => Some('.')
        
        | ASTERISK => Some('*')
        | BANG => Some('!')
        | QUESTION => Some('?')
        | TILDE => Some('~')
        | POUND => Some('#')
        | AT => Some('@')
        | AMP => Some('&')
        | DOLLAR => Some('$')
        
        | EOF => Some('\000')
        | _ => None
    }
}

let of_char = (c: char): t => {
    switch c {
    | '(' => LPAREN
    | ')' => RPAREN
    | '[' => LBRACK
    | ']' => RBRACK
    | '{' => LSQUIRLY
    | '}' => RSQUIRLY

    | '=' => ASSIGN

    | '+' => PLUS
    | '-' => MINUS
    | '%' => MODULO
    | '^' => CARET

    | '/' => FORSLASH
    | '\\' => BACKSLASH

    | '\'' => SINGLEQUOTE
    | '\"' => DOUBLEQUOTE
    | '`' => BACKTICK

    | '>' => GREATER
    | '<' => LESSER
    
    | ';' => SEMICOLON
    | ':' => COLON
    | '|' => PIPE
    | ',' => COMMA
    | '.' => DOT
    
    | '*' => ASTERISK
    | '!' => BANG
    | '?' => QUESTION
    | '~' => TILDE
    | '#' => POUND
    | '@' => AT
    | '&' => AMP
    | '$' => DOLLAR
    
    | '\000' => EOF
    | _ => ILLEGAL
    }

}

let parse_keyword = (s: string) => {
    switch s {
        | "fn" => Some(FN)
        | "let" => Some(LET)
        | "bind" => Some(BIND)
        | "match" => Some(MATCH)
        | "true" => Some(TRUE)
        | "false" => Some(FALSE)
        | "return" => Some(RETURN)
        | "if" => Some(IF)
        | "else" => Some(ELSE)
        | _ => None
    }
}
