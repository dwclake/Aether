open Stdio

let flush_out () = Out_channel.flush Out_channel.stdout;;

let prompt = ">> "

let lex_input (input: string): Token.t list =
    let tokens = ref [] in
    let lex = ref
        (Lexer.next_token(Lexer.create ~input))
    in
    while (!lex#tok != Token.EOF) do
        tokens := !tokens @ [!lex#tok];

        lex := (Lexer.next_token !lex#l);
    done;
    !tokens;
;;

let print (token: Token.t) =
    printf "\t%s\n" (Token.show token)
;;

let print_toks (tokens: Token.t list): unit =
    printf "{\n";
    tokens |> Core.List.iter ~f:print;
    printf "}\n"
;;

let rec start () =
    let open Core in
    printf "\n%s" prompt; flush_out();

    let input = In_channel.input_lines In_channel.stdin in

    let tokens = List.fold input ~init:"" ~f:(fun x accum -> x ^ accum)
        |> lex_input
    in

    printf "\n";
    print_toks tokens;
    start()
;;
