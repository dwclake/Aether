type t = {
    input: string;
    pos: int;
    read_pos: int;
    ch: char;
};;

type ('a) lex_r = <
    l: t;
    ..
> as 'a;;

let create ~(input: string) =
    let ch = match input with
        | "" -> '\000'
        | _ -> input.[0]
    in

    {   input;
        pos = 0;
        read_pos = 1;
        ch;
    }
;;

let advance ?(count = 1) (l: t): t =
    let rp = l.pos + count in
    let ch = if rp >= String.length l.input then 
            '\000'
        else 
            String.get l.input rp
    in

    {   l with
        pos = rp;
        read_pos = rp + 1;
        ch;
    }
;;

let peek (l: t): char =
    let ch = if l.read_pos >= String.length l.input then 
            '\000'
        else 
            String.get l.input l.read_pos
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
    | ch when is_letter ch -> true
    | ch when is_number ch -> true
    | _ -> false
;;

let rec read_sequence ?(s = "") ~predicate (l: t): <literal: string; ..> lex_r =
    match l.ch with
        | ch when predicate ch ->
            let literal = s ^ Core.Char.to_string ch in
            let l = advance l in

            read_sequence l ~predicate ~s:literal
        | _ ->
            object
                method l = l;
                method literal = s;
            end
;;

let compound_or (l: t) ~(default: Token.t) ~(rules): <tok: Token.t; ..> lex_r =
    let next_ch = peek l in
    
    let rec loop = function
        | [] -> default 
        | h::t ->
            let (ch, tok) = h in

            if next_ch == ch then 
                tok
            else 
                loop t
    in
    let tok = loop rules in

    match tok with
    | tok when tok == default ->
        object
            method l = advance l;
            method tok = tok;
        end
    | _ ->
        object
            method l = advance l ~count:2;
            method tok = tok;
        end
;;

let next_token (l: t): <tok: Token.t; ..> lex_r =
    let l = skip_whitespace l in

    match l.ch with
        (* Idenifiers and keywords *)
        | ch when is_letter ch ->
            let lex = read_sequence l ~predicate:is_alphanumeric in
            let token = match Token.try_keyword lex#literal with
                | Some t -> t
                | None -> Token.IDENT(lex#literal)
            in

            object
                method l = lex#l;
                method tok = token;
            end
        (* Integers *)
        | ch when is_number ch ->
            let lex = read_sequence l ~predicate:is_number in 
            
            object
                method l = lex#l;
                method tok = Token.INT(lex#literal);
            end
        (* Compound operators *)
        | ch when ch == '>' ->
            l |> compound_or 
                ~default:Token.GREATER 
                ~rules:[('=', Token.GREATEREQ)] 
        | ch when ch == '<' ->
            l |> compound_or
                ~default:Token.LESSER 
                ~rules:[('=', Token.LESSEREQ)]
        | ch when ch == '!' ->
            l |> compound_or 
                ~default:Token.BANG 
                ~rules:[('=', Token.NOTEQ)]
        | ch when ch == '-' ->
            l |> compound_or 
                ~default:Token.MINUS 
                ~rules:[('>', Token.SLIMARROW)]
        | ch when ch == '=' ->
            l |> compound_or 
                ~default:Token.ASSIGN 
                ~rules:[
                    ('=', Token.EQUALS);
                    ('>', Token.FATARROW)
                ]
        (* Individual characters *)
        | ch ->
            object
                method l = advance l;
                method tok = Token.of_char ch;
            end
;;
