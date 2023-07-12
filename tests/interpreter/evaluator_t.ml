open Briar_i
open Alcotest

let unwrap result = match result with
    | Ok (_, program) -> program
    | Error (_, message) -> failwith message
;;

let rec test_int_seq ?(i=1) lists = match lists with
    | ([], []) -> ()
    | (es::et, s::t) ->
        check Alcotest.int (string_of_int i) es s;

        test_int_seq (et, t) ~i:(i + 1);
    | _ -> failwith "Lists must be of the same size"
;;

let test_int_eval () =
    let output = "
        5;
        10;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
        |> unwrap
        |> Evaluator.eval
    in
    ([5; 10], output)
    |> test_int_seq
;;
