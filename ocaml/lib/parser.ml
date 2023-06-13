type t = {
    l: Lexer.t;

    cur_t: Token.t;
    peek_t: Token.t;
};;

type par_r = <
    p: t;
    stmt: (Ast.statement, string) result;
> ;;

(* Operator overload for creating a new par_r *)
let (++) p stmt: par_r =
    object
        method p = p;
        method stmt = stmt;
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
    } |> next_token |> next_token
;;

let parse_let_statement (p: t): par_r =
    begin match p.peek_t with
        | IDENT s -> begin
            let open Ast in

            let p = next_token p in
            let name = {identifier=s} in

            match p.peek_t with
                | ASSIGN -> begin
                    (*expressions will be parsed here later*)
                    let rec loop (p: t) =
                        match p.cur_t with
                        | SEMICOLON -> p
                        | _ -> loop (next_token p)
                    in

                    let p = loop p in
                    (++) p (Ok (LET{
                        name=name; 
                        value=IDENTIFIER name
                    }))
                end
                | _ -> (++) p (Error "identifier must be followed by an =")
        end
        | _ -> (++) p (Error "let must be followed by an identifier");
    end
;;

let parse_statement (p: t): par_r =
    match p.cur_t with
        | Token.LET -> parse_let_statement p;
        | _ -> (++) p (Error "Only supports let statements currently")
;;

let parse_program (p: t): (Ast.program, string) result =
    let rec loop p stmts =
        match p.cur_t with
        | Token.EOF -> stmts
        | _ ->
            let par = parse_statement p in
            match par#stmt with
                | Ok s -> loop (next_token par#p) (stmts @ [s])
                | Error e ->
                    Stdio.eprintf "%s\n" e;
                    loop (next_token par#p) stmts
    in

    let statements = loop p [] in
    match statements with
        | [] -> Error "No statements in program"
        | _ -> Ok {
            statements
        }
;;
