open Stdio;

let flush = () => Out_channel.flush(Out_channel.stdout);

let prompt = ">> ";

type lex = {
    l: Lexer.t,
    t: Token.t
}

let of_tuple = ((l, t)): lex => {
    { l, t }
}

let lex_input = (input: string): list(Token.t) => {
    let tokens = ref{[]};
    let lex = ref(
        of_tuple(Lexer.next_token(Lexer.create(~input)))
    );

    while(lex^.t != Token.EOF){
        tokens := tokens^ @ [lex^.t];

        lex := of_tuple(Lexer.next_token(lex^.l));
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
    printf("\n%s", prompt); flush();

    let input = In_channel.input_line(In_channel.stdin);
    let tokens = switch input {
        | Some(s) => lex_input(s)
        | None => [Token.EOF]
    };

    printf("\n");
    print_toks(tokens);
    start()
}
