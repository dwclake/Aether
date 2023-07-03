class t = fun ~input -> 
    let ch = match input with
        | "" -> '\000'
        | _ -> input.[0]
    in object
        val input' = input
        method input = input'
        
        val mutable pos': int = 0
        method pos = pos'
        method set_pos pos = pos' <- pos
        
        val mutable read_pos': int = 1
        method read_pos = read_pos'
        method set_read_pos pos = read_pos' <- pos
        
        val mutable ch': char = ch
        method ch = ch'
        method set_ch ch = ch' <- ch
end;;

let advance ?(count=1) lexer =
    let rp = !lexer#pos + count in
    let ch = if rp >= String.length !lexer#input
        then '\000'
        else String.get !lexer#input rp
    in

    !lexer#set_pos rp;
    !lexer#set_read_pos @@ rp + 1;
    !lexer#set_ch ch;
;;

let peek lexer =
    if !lexer#read_pos >= String.length !lexer#input
        then '\000'
        else String.get !lexer#input !lexer#read_pos
;;

let rec skip_whitespace lexer =
    match !lexer#ch with
        | ' ' | '\t' | '\n' | '\r' ->
            advance lexer;
            skip_whitespace lexer;
        | _ -> ()
;;

let is_letter = function
    | 'a'..'z' | 'A'..'Z' | '_' -> true
    | _ -> false
;;

let is_integer = function
    | '0'..'9' -> true
    | _ -> false
;;

let is_number = function
    | '0'..'9' | '.' -> true
    | _ -> false
;;

let is_alphanumeric = function
    | ch when is_letter ch -> true
    | ch when is_integer ch -> true
    | _ -> false
;;

let rec read_sequence ?(acc="") ~predicate lexer =
    match !lexer#ch with
        | ch when predicate ch ->
            let literal = acc ^ Core.Char.to_string ch in
            let () = advance lexer in

            read_sequence lexer ~predicate ~acc:literal
        | _ -> acc
;;

let compound_or ~default ~rules lexer =
    let next_ch = peek lexer in
    
    let rec loop = function
        | [] -> default 
        | h::t ->
            let (ch, tok) = h in
            if next_ch == ch then 
                tok
            else 
                loop t
    in
    let token = loop rules in

    match token with
    | token when token == default ->
            let () = advance lexer in
            token
    | _ ->
            let () = advance lexer ~count:2 in
            token
;;

let next_token lexer =
    let () = skip_whitespace lexer in

    match !lexer#ch with
        (* Idenifiers and keywords *)
        | ch when is_letter ch ->
            let literal = read_sequence lexer ~predicate:is_alphanumeric in
            begin match Token.try_keyword literal with
                | Some token -> token
                | None -> Token.Ident(literal)
            end
        (* Integers *)
        | ch when is_integer ch ->
            let literal = read_sequence lexer ~predicate:is_number in 
            if String.exists (fun ch -> ch == '.') literal 
                then Token.Float literal
                else Token.Int literal
        (* Compound operators *)
        | ch when ch == '>' ->
            lexer |> compound_or 
                ~default:Token.Gt
                ~rules:[('=', Token.Geq)] 
        | ch when ch == '<' ->
            lexer |> compound_or
                ~default:Token.Lt
                ~rules:[('=', Token.Leq)]
        | ch when ch == '!' ->
            lexer |> compound_or 
                ~default:Token.Bang 
                ~rules:[('=', Token.Neq)]
        | ch when ch == '-' ->
            lexer |> compound_or 
                ~default:Token.Minus 
                ~rules:[('>', Token.Arrow)]
        | ch when ch == '|' ->
            lexer |> compound_or 
                ~default:Token.Pipe
                ~rules:[('|', Token.Or)]
        | ch when ch == '&' ->
            lexer |> compound_or 
                ~default:Token.Ampersand 
                ~rules:[('&', Token.And)]
        | ch when ch == '=' ->
            lexer |> compound_or 
                ~default:Token.Assign 
                ~rules:[('=', Token.Eq); ('>', Token.FatArrow)]
        (* Individual characters *)
        | ch ->
                let () = advance lexer in
            Token.of_char ch
;;
