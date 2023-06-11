open Stdio;

let flush_out = () => Out_channel.flush(Out_channel.stdout);

let prompt = ">> ";

let lex_input = (input: string): list(Token.t) => {
    let tokens = ref{[]};
    let lex = ref(
        Lexer.next_token(Lexer.create(~input))
    );

    while(lex^.t != Token.EOF) {
        tokens := tokens^ @ [lex^.t];

        lex := Lexer.next_token(lex^.l);
    }

    tokens^
}

let print = (token: Token.t) => {
    printf("\t%s\n", Token.show(token));
}

let print_toks = (tokens: list(Token.t)): unit => {
    printf("{\n");
    tokens |> Core.List.iter(~f=print);
    printf("}\n")
}

let rec start = () => {
    open Core;

    printf("\n%s", prompt); flush_out();

    let input = In_channel.input_lines(In_channel.stdin);

    let tokens = List.fold(input, ~init="", ~f=((x, accum) => x ++ accum)) 
        |> lex_input;

    printf("\n");
    print_toks(tokens);
    start()
}
