type t =
| IDENT(string)
| INT(string)

| FN

| L_PAREN
| R_PAREN
| L_BRACKET
| R_BRACKET
| L_SQUIRELY
| R_SQUIRELY

| LET
| BIND
| ASSIGN

| EQUALS
| NOT_EQUALS
| PLUS
| MINUS

| FORWARD_SLASH
| BACK_SLASH

| SINGLE_QUOTE
| DOUBLE_QUOTE
| BACK_TICK

| GREATER
| LESSER

| SEMICOLON
| COLON
| PIPE
| COMMA
| DOT

| ASTERISK
| BANG
| QUESTION
| TILDE

| ILLEGAL
| EOF

let to_string = (token: t): string => {
    switch token {
        | IDENT(id) => id
        | INT(x) => x

        | FN => "fn"

        | L_PAREN => "("
        | R_PAREN => ")"
        | L_BRACKET => "["
        | R_BRACKET => "]"
        | L_SQUIRELY => "{"
        | R_SQUIRELY => "}"

        | LET => "let"
        | BIND => "bind"
        | ASSIGN => "="

        | EQUALS => "=="
        | NOT_EQUALS => "!="
        | PLUS => "+"
        | MINUS => "-"

        | FORWARD_SLASH => "/"
        | BACK_SLASH => "\\"

        | SINGLE_QUOTE => "'"
        | DOUBLE_QUOTE => "\""
        | BACK_TICK => "`"

        | GREATER => ">"
        | LESSER => "<"
        
        | SEMICOLON => ";"
        | COLON => ":"
        | PIPE => "|"
        | COMMA => ","
        | DOT => "."
        
        | ASTERISK => "*"
        | BANG => "!"
        | QUESTION => "?"
        | TILDE => "~"
        
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

    | '=' => 
        if (true) {
            ASSIGN 
        } else if (true){
            EQUALS 
        } else {
            NOT_EQUALS
    };
    | '+' => PLUS
    | '-' => MINUS

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
    
    | '\000' => EOF
    | _ => ILLEGAL
    }

}

let keyword = () => {

}
