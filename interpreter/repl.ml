open Stdio

let logo = "
    _,--._.-,
   /\\_--,\\_ )
.-.) _;='_/ (.;
 \\  \\'     \\/ )
  \\.'-. _.'|-'
  <`-'\'_.'/'-`
   `'-._( \\ )
     ___   \\,      ___
     \\ .'-. \\   .-'_. /
      '._' '.\\//.-'_.'
         '--``\\('--'
               \\
               `\\,
                 \\
";;

let intro = Format.sprintf 
"Welcome to the Briar repl
%s
Enter to continue typing on a new line.
Ctrl+d on a emply line to submit.
Enter exit or press Ctrl+c to exit.
"
logo
;;

let prompt = ":> "

let flush_out () = Out_channel.flush Out_channel.stdout;;

(* Ctrl+d on a empty line to end input *)
let start () =
    let open Core in
    
    let _ = Stdlib.Sys.command "clear" in
    let () = printf "%s\n" intro in
    
    let rec loop () = 
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
        in
        match program with
            | "exit" -> ()
            | _ ->
                let program = 
                    program
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
                loop()
        in
        loop()
;;
