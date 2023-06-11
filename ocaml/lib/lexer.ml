type t = {
    input: string;
    pos: int;
    read_pos: int;
    ch: char;
};;

class ['a] lex = fun lexer content ->
    object
        method l: t = lexer;
        method content: 'a = content;
    end

let create ~(input: string) =
    let ch = match input with
        | "" -> '\000'
        | _ -> input.[0]
    in
    {
        input;
        pos = 0;
        read_pos = 1;
        ch;
    }
;;

let advance ?(count = 1) (l: t): t =
    let rp = l.pos + count in
    let ch = if rp >= String.length l.input
             then '\000'
             else String.get l.input rp
    in
    {
        l with
        pos = rp;
        read_pos = rp + 1;
        ch;
    }
;;

let peek (l: t): char =
    let ch = if l.read_pos >= String.length l.input 
             then '\000'
             else String.get l.input l.read_pos
    in
    ch
;;

let rec skip_whitespace (l: t) =
    match l.ch with
        | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (advance l)
        | _ -> l
;;

let is_letter = function
    | 'a'..'z' | 'A'..'Z' | '_' -> true
    | _ -> false
;;

let is_number = function
    | '0'..'9' -> true
    | _ -> false
;;

let is_alphanumeric = function
    | ch when is_letter(ch) -> true
    | ch when is_number(ch) -> true
    | _ -> false
;;

let rec read_sequence ?(s = "") ~predicate (l: t): string lex =
    match l.ch with
        | ch when predicate ch ->
            let literal = s ^ Core.Char.to_string ch in
            let l = advance l in
            read_sequence l ~predicate ~s:literal
        | _ ->
            new lex l s
;;

let compound_or (l: t) ~(default: Token.t) ~(rules): Token.t lex =
    let next_ch = peek(l) in
    
    let rec loop = function
        | [] -> default 
        | h :: t ->
            let (ch, tok) = h in
            if next_ch == ch
            then tok
            else loop t
    in
    let tok = loop rules in

    match tok with
        | tok when tok == default ->
            new lex (advance l) tok
        | _ ->
            new lex (advance l ~count:2) tok
;;

let next_token (l: t): Token.t lex =
    let l = skip_whitespace l in

    match l.ch with
        (* Idenifiers and keywords *)
        | ch when is_letter ch ->
                let lex = read_sequence l ~predicate:is_alphanumeric in
            let token = match Token.parse_keyword lex#content with
                | Some(t) -> t
                | None -> Token.IDENT(lex#content)
            in
            new lex lex#l token
        (* Integers *)
        | ch when is_number ch ->
            let lex = 
                read_sequence l ~predicate:is_number in
            new lex lex#l (Token.INT(lex#content))
        (* Compound operators *)
        | ch when ch == '>' ->
            compound_or 
                l 
                ~default:Token.GREATER 
                ~rules:[('=', Token.GREATEREQ)]
        | ch when ch == '<' ->
            compound_or 
                l 
                ~default:Token.LESSER 
                ~rules:[('=', Token.LESSEREQ)]
        | ch when ch == '!' ->
            compound_or 
                l  
                ~default:Token.BANG 
                ~rules:[('=', Token.NOTEQ)]
        | ch when ch == '-' ->
            compound_or 
                l 
                ~default:Token.MINUS 
                ~rules:[('>', Token.SLIMARROW)]
        | ch when ch == '=' ->
            compound_or 
                l 
                ~default:Token.ASSIGN 
                ~rules:[
                    ('=', Token.EQUALS);
                    ('>', Token.FATARROW)
                ]
        (* Individual characters *)
        | ch -> 
            new lex (advance l) (Token.of_char ch)
;;
