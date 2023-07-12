let main () = 
    Clap.description "Welcome to Briar!";
    let script = Clap.section "SCRIPT" in
    let command =
        Clap.subcommand 
        [(Clap.case "repl" ~description: "Starts the interactive repl" @@ fun () -> `repl)
        ;(Clap.case "script" ~description: "Interprets the file given as a argument" @@ fun () ->
            let source = 
                Clap.mandatory_string 
                ~section: script 
                ~placeholder: "FILENAME" 
                () 
            in
            `script(source)
         )
        ]
    in
    Clap.close();

    match command with
        | `repl -> 
            Briar_i.Repl.start()
        | `script file -> 
            begin match Briar_i.Script.run_script file with
                | Ok (program) -> Stdio.print_endline program
                | Error (error) -> Stdio.prerr_endline error
            end
;;

let () = main ();;
