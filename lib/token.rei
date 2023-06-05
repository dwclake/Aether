[@deriving show]
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
| EOF;

let of_char: char => t;
let to_string: t => string;
let to_char: t => option(char);
let keyword: string => t;
