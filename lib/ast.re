module Node = {
    type t = {
        token: Token.t
    };

    let token_literal = (n: t): string => {
        Token.show(n.token)
    };
};



module Expression = {
    type t;

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

module LetStatement = {
    type t = {
        token: Token.t,
        name: Identifier.t,
        value: string
    };
};

module Statement  = {
    type t =
        | LetStatement(
            Token.t,
            Identifier.t,
            Expression.t
        )
    ;

    let statement_node = (_: t) => ();

    let token_literal = (l: t): string => {
        let token = switch l {
            | LetStatement(t, _, _) => t
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
