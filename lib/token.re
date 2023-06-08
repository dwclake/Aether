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
    | ELSE_IF

    | L_PAREN
    | R_PAREN
    | L_BRACKET
    | R_BRACKET
    | L_SQUIRELY
    | R_SQUIRELY

    | ASSIGN
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
        | ELSE_IF => "else if"

        | L_PAREN => "("
        | R_PAREN => ")"
        | L_BRACKET => "["
        | R_BRACKET => "]"
        | L_SQUIRELY => "{"
        | R_SQUIRELY => "}"

        | ASSIGN => "="
        | FAT_ARROW => "=>"

        | EQUALS => "=="
        | NOT_EQUALS => "!="

        | PLUS => "+"
        | MINUS => "-"
        | MODULO => "%"
        | CARET => "^"

        | FORWARD_SLASH => "/"
        | BACK_SLASH => "\\"

        | SINGLE_QUOTE => "'"
        | DOUBLE_QUOTE => "\""
        | BACK_TICK => "`"

        | GREATER => ">"
        | LESSER => "<"
        
        | GREATER_EQ => ">="
        | LESSER_EQ => "<="

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
        | L_PAREN => Some('(')
        | R_PAREN => Some(')')
        | L_BRACKET => Some('[')
        | R_BRACKET => Some(']')
        | L_SQUIRELY => Some('{')
        | R_SQUIRELY => Some('}')

        | ASSIGN => Some('=')

        | PLUS => Some('+')
        | MINUS => Some('-')
        | MODULO => Some('%')
        | CARET => Some('^')

        | FORWARD_SLASH => Some('/')
        | BACK_SLASH => Some('\\')

        | SINGLE_QUOTE => Some('\'')
        | DOUBLE_QUOTE => Some('\"')
        | BACK_TICK => Some('`')

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
    | '(' => L_PAREN
    | ')' => R_PAREN
    | '[' => L_BRACKET
    | ']' => R_BRACKET
    | '{' => L_SQUIRELY
    | '}' => R_SQUIRELY

    | '+' => PLUS
    | '-' => MINUS
    | '%' => MODULO
    | '^' => CARET

    | '/' => FORWARD_SLASH
    | '\\' => BACK_SLASH

    | '\'' => SINGLE_QUOTE
    | '\"' => DOUBLE_QUOTE
    | '`' => BACK_TICK

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
        | "else if" => Some(ELSE_IF)
        | _ => None
    }
}
