module Node = {
    type t = {
        token: Token.t
    };

    let token_literal = (n: t): string => {
        Token.show(n.token)
    };
};

module Expression = {
    type t = 
        | IDENT(
            Token.t,
            string
        )
    ;

    let expression_node = (_: t) => ();

    let token_literal = (e: t): string => {
        let token = switch e {
            | IDENT(t, _) => t
        };
        Token.show(token)
    };
};

module Identifier = {
    type t = {
        token: Token.t,
        value: string
    };

    let expression_node = (_: t) => ();

    let token_literal = (i: t): string => {
        Token.show(i.token)
    };
};

module Statement  = {
    type t =
        | LET(
            Token.t,
            Identifier.t,
            Expression.t
        )
    ;

    let statement_node = (_: t) => ();

    let token_literal = (l: t): string => {
        let token = switch l {
            | LET(t, _, _) => t
        };
        Token.show(token)
    };
};


module Program = {
    type t = {
        statements: list(Statement.t)
    };

    let token_literal = (p: t): string => {
        if (List.length(p.statements) > 0) {
            Statement.token_literal(List.nth(p.statements, 0))
        } else {
            ""
        }
    };
};