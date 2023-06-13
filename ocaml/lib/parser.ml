type t = {
    l: Lexer.t;

    cur_t: Token.t;
    peek_t: Token.t;
};;

class par_r = fun parser stmt ->
    object
        method p: t = parser;
        method stmt: (Ast.statement, string) result = stmt;
    end
;;

let next_token (p: t): t =
    let lex = Lexer.next_token p.l in
    {
        l = lex#l;
        cur_t = p.peek_t;
        peek_t = lex#tok;
    }
;;

let create (l: Lexer.t): t =
    {
        l;
        cur_t = Token.EOF;
        peek_t = Token.EOF;
    } 
    |> next_token 
    |> next_token
;;

let parse_let_statement (p: t): par_r =
    new par_r p (Error "Unimplemented")
;;

let parse_statement (p: t): par_r =
    match p.cur_t with
        | Token.LET -> parse_let_statement p;
        | _ -> new par_r p (Error "Only supports let statements currently")
;;

let parse_program (p: t): (Ast.program, string) result =
    let rec loop (p: t) (stmts): Ast.statement list = begin
        match p.cur_t with
        | Token.EOF -> stmts
        | _ -> (
            let par = parse_statement p in
            match par#stmt with
                | Ok s -> loop (par#p |> next_token) (stmts @ [s])
                | Error e -> (
                    Stdio.eprintf "%s\n" e;
                    loop (par#p |> next_token) stmts
                )
        )
    end in

    let statements = loop p [] in
    match statements with
        | [] -> Error "No statements in program"
        | _ -> Ok {
            statements
        }
;;
