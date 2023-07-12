open Core

let run_script file =
    let input = 
        Stdio.In_channel.read_all file
        |> String.split ~on:'\n' 
    in
    let program = 
        List.fold 
            input 
            ~init:"" 
            ~f:(fun x accum -> x ^ accum)
    in
    let program = 
        program
        |> new Lexer.t |> ref
        |> Parser.create
        |> Parser.parse_program
    in
    match program with
        | Ok (_, program) -> Ok (program.statements |> Ast.string)
        | Error (_, message) -> Error ("Error: " ^ message)
;;
