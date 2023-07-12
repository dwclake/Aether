open Stdio

let flush_out () = Out_channel.flush Out_channel.stdout;;

let prompt = ">> "

(*
let lex_input input =
    let tokens = ref [] in
    let lexer =
        input
        |> new Lexer.t
        |> ref
    in
    let token =
        lexer
        |> Lexer.next_token
        |> ref
    in
    while (!token != Token.Eof) do
        tokens := [!token] @ !tokens;

        token := Lexer.next_token lexer;
    done;

    !tokens |> List.rev
;;
*)

let unwrap result = match result with
    | Ok (_, program) -> program
    | Error (_, message) -> failwith message
;;

let rec start () =
    let open Core in
    
    let () = 
        printf "%s" prompt; 
        flush_out();
    in

    let input = In_channel.input_lines In_channel.stdin in
    let program = 
        List.fold 
            input 
            ~init:"" 
            ~f:(fun x accum -> x ^ accum)
        |> new Lexer.t |> ref
        |> Parser.create
        |> Parser.parse_program
        |> unwrap
    in
    let program = program.statements
        |> Ast.string
    in

    let () = 
        printf "\n";
        printf "%s\n" program;
    in    
    start()
;;
