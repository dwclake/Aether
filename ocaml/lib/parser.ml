type t = {
    l: Lexer.t;
    errors: string list;

    cur_t: Token.t;
    peek_t: Token.t;
};;

type par_r = {
    p: t;
    stmt: Ast.statement option;
};;

let next_token (p: t): t =
    let lex = Lexer.next_token p.l in
    {
        l = lex#l;
        errors = p.errors;
        cur_t = p.peek_t;
        peek_t = lex#tok;
    }
;;

let create (l: Lexer.t): t =
    {
        l;
        errors = [];
        cur_t = Token.EOF;
        peek_t = Token.EOF;
    } 
    |> next_token |> next_token
;;


let peek_error (p: t)(t: Token.t): t =
    let error = Format.sprintf 
        "Expected next token to be %s, got %s instead" 
        (Token.show t) 
        (Token.show p.peek_t)
    in

    {p with errors = p.errors @ [error]}
;;

(*
let expect_token (p: t)(t: Token.t) = 
    let a = Obj.repr(p.peek_t) in
    let b = Obj.repr(t) in

    let res = match (Obj.is_block a, Obj.is_block b) with
        | (true, true) -> (Obj.tag a) == (Obj.tag b)
        | (false, false) -> a == b
        | _ -> false
    in
    if res then (
        let p = next_token p in
        (p, true)
    )
    else (
        let p = peek_error p t in
        (p, false)
    )
;;

let parse_let_statement (p: t): par_r =
    let (p, res) = expect_token p (Token.IDENT "") in
    
    if not res then
        (++) p None
    else (
        let open Ast in
        let name = match p.cur_t with
            | IDENT s -> {identifier=s}
            | _ -> {identifier=""}
        in
        let (p, res) = expect_token p Token.ASSIGN in
        
        if not res then
            (++) p None
        else (
            (*expressions will be parsed here later*)
            let rec loop (p: t) =
                match p.cur_t with
                | SEMICOLON -> p
                | _ -> loop (next_token p)
            in
            let p = loop p in

            (++) p (Some (LET{
                name=name; 
                value=IDENTIFIER name
            }))
        )
    )
;;
*)

let parse_let_statement (p: t): par_r =
    begin match p.peek_t with
        | IDENT s -> (
            let open Ast in

            let p = next_token p in
            let name = {identifier=s} in

            match p.peek_t with
                | ASSIGN -> (
                    (*expressions will be parsed here later*)
                    let rec loop (p: t) =
                        match p.cur_t with
                        | SEMICOLON -> p
                        | _ -> loop (next_token p)
                    in

                    let p = loop p in
                    {p; stmt=(Some (LET{name=name; value=IDENTIFIER name}))}
                )
                | _ -> let p = peek_error p (Token.ASSIGN) in
                       {p; stmt=None}
        )
        | _ -> let p = peek_error p (Token.IDENT "") in
               {p; stmt=None}
    end
;;

let parse_statement (p: t): par_r =
    match p.cur_t with
        | Token.LET -> parse_let_statement p;
        | _ -> {p; stmt=None}
;;

let parse_program (p: t): (t * Ast.program) =
    let rec loop p stmts =
        match p.cur_t with
        | Token.EOF -> (p, stmts)
        | _ -> (
            let par = parse_statement p in
            match par.stmt with
                | Some s -> loop (next_token par.p) (stmts @ [s])
                | None -> loop (next_token par.p) stmts
        )
    in
    let (p, statements) = loop p [] in

    (p, {statements=statements})
;;
