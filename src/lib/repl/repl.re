open Stdio;

let flush_out() = Out_channel.flush(Out_channel.stdout);

let prompt = ">> ";

let lex_input(input: string): list(Token.t) = {
    let tokens = ref{[]};
    let lex = 
        Lexer.create(~input)
        |> Lexer.next_token
        |> ref;

    while((lex^)#token != Token.Eof) {
        tokens := [(lex^)#token] @ tokens^;

        lex := Lexer.next_token((lex^)#lexer);
    }

    tokens^ |> List.rev
};

let print(token: Token.t): unit = {
    printf("\t%s\n", Token.show(token));
};

let print_toks(tokens: list(Token.t)): unit = {
    printf("{\n");
    tokens |> Core.List.iter(~f=print);
    printf("}\n");
};

let rec start(): unit = {
    open Core;

    printf("\n%s", prompt); 
    flush_out();

    let input = In_channel.input_lines(In_channel.stdin);

    let tokens = 
        List.fold(input, ~init="", ~f=((x, accum) => x ++ accum)) 
        |> lex_input;

    
    printf("\n");
    print_toks(tokens);

    start()
};
