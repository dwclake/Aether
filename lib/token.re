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

let from_string = (s: string): t => {
    switch s {
    | "hello" => IDENT("")
    | "5" => INT("")

    | "fn" => FN

    | "(" => L_PAREN
    | ")" => R_PAREN
    | "[" => L_BRACKET
    | "]" => R_BRACKET
    | "{" => L_SQUIRELY
    | "}" => R_SQUIRELY

    | "let" => LET
    | "bind" => BIND
    | "=" => 
        if (true) {
            ASSIGN 
        } else if (true){
            EQUALS 
        } else {
            NOT_EQUALS
    };
    | "+" => PLUS
    | "-" => MINUS

    | "/" => FORWARD_SLASH
    | "\\" => BACK_SLASH

    | "'" => SINGLE_QUOTE
    | "\"" => DOUBLE_QUOTE
    | "`" => BACK_TICK

    | ">" => GREATER
    | "<" => LESSER
    
    | ";" => SEMICOLON
    | ":" => COLON
    | "|" => PIPE
    | "," => COMMA
    | "." => DOT
    
    | "*" => ASTERISK
    | "!" => BANG
    | "?" => QUESTION
    | "~" => TILDE
    
    | "" => EOF
    | _ => ILLEGAL
    }

}

let keyword = () => {

}
