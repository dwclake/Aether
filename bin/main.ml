(* Ctrl+d on a empty line to end input *)

let main () = 
    let args = Sys.argv in
    match args with
        | [|_; "repl"|] ->
            Briar.Repl.start()
        | [|_; "script"; file|] ->
            begin match Briar.Script.run_script file with
                | Ok (program) -> Stdio.print_endline program
                | Error (error) -> Stdio.prerr_endline error
            end
        | _ -> Stdio.printf "
Welcome to Briar!

usage: 
    briar <cmd> <args>

<cmd>'s:
    repl - starts repl
    script <filename> - interprets file
"
;;

let _ = main ();;
