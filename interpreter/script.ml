open Core

let run_script file =
    let program = 
        Stdio.In_channel.read_all file
        |> new Lexer.t |> ref
        |> Parser.create
        |> Parser.parse_program
    in
    match program with
        | Ok (_, program) -> Ok (program.statements |> Ast.string)
        | Error (_, message) -> Error ("Error: " ^ message)
;;