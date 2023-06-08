open Core
open Stdio

let rec read_and_accumulate = (accum) => {
    let line = In_channel.input_line(In_channel.stdin);
    switch line {
        | None => accum
        | Some(x) => read_and_accumulate(accum +. Float.of_string(x))
    }
};

let () =
    printf("Total: %F\n", (read_and_accumulate(0.)))
