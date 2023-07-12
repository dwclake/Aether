open Stdio

let flush_out () = Out_channel.flush Out_channel.stdout;;

let prompt = ">> "

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
    in
    let program = match program with
        | Ok (_, program) -> program.statements |> Ast.string
        | Error (_, message) -> "Error: " ^ message
    in

    let () = 
        printf "\n";
        printf "%s\n" program;
    in    
    start()
;;
