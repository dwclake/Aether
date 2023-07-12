(* Ctrl+d on a empty line to end input *)

let logo = "
    _,--._.-,
   /\\_--,\\_ )
.-.) _;='_/ (.;
 \\  \\'     \\/ )
  \\.'-. _.'|-'
 <_`-'\'_.'/'-/`>
   `'-._( \\ )
     ___   \\,      ___
     \\ .'-. \\   .-'_. /
      '._' '.\\//.-'_.'
         '--``\\('--'
               \\
               `\\,
                 \\
"

let intro = Format.sprintf 
"Welcome to the Briar repl
%s
Enter to continue typing on a new line.
Ctrl+d on a emply line to submit.
Enter exit or press Ctrl+c to exit.
    "
    logo

let main () = 
    let _ = Sys.command "clear" in
    Stdio.printf "%s\n" intro;
    Briar.Repl.start()
;;

let _ = main ();;
