open Stdio

let flush_out () = Out_channel.flush Out_channel.stdout;;

let prompt = ">> "

let lex_input (input: string): Token.t list =
    let tokens = ref [] in
    let lex =
        Lexer.create ~input 
        |> Lexer.next_token 
        |> ref
    in
    while (!lex#tok != Token.EOF) do
        tokens := [!lex#tok] @ !tokens;

        lex := (Lexer.next_token !lex#l);
    done;

    !tokens |> List.rev
;;

let print (token: Token.t) =
    printf "\t%s\n" (Token.show token)
;;

let print_toks (tokens: Token.t list): unit = 
    printf "{\n";
    tokens |> Core.List.iter ~f:print;
    printf "}\n";
;;

let rec start () =
    let open Core in
    let () = 
        printf "\n%s" prompt; 
        flush_out();
    in

    let input = In_channel.input_lines In_channel.stdin in
    let tokens = 
        List.fold 
            input 
            ~init:"" 
            ~f:(fun x accum -> x ^ accum)
        |> lex_input
    in

    let () = 
        printf "\n";
        print_toks tokens;
    in

    start()
;;
