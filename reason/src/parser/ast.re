[@deriving (show, eq)]
type node = 
    | PROGRAM(program)
    | EXPRESSION(expression)
    | STATEMENT(statement)

    and expression = 
        | IDENTIFIER(identifier)

    and statement =
        | LET {
            name: identifier,
            value: expression
        }

    and identifier = {
        identifier: string
    }

    and program = {
        statements: list(node)
};

let token_literal = { fun
    | PROGRAM(_) => "program"
    | EXPRESSION(_) => "expression"
    | STATEMENT(_) => "statement"
};
