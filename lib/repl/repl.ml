open Stdio

let flush_out () = Out_channel.flush Out_channel.stdout;;

let prompt = ">> "

let lex_input input =
    let tokens = ref [] in
    let lexer =
        new Lexer.t ~input 
        |> ref
    in
    let token =
        Lexer.next_token lexer
        |> ref
    in
    while (!token != Token.Eof) do
        tokens := [!token] @ !tokens;

        token := Lexer.next_token lexer;
    done;

    !tokens |> List.rev
;;

let print token =
    printf "\t%s\n" @@ Token.show token
;;

let print_toks tokens = 
    printf "{\n";
    tokens |> Core.List.iter ~f:print;
    printf "}\n";
;;

let rec start () =
    let open Core in
    
    let () = 
        printf "%s" prompt; 
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
