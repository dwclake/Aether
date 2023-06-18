[@deriving (show, eq)]
type node = 
    | Program(program)
    | Expression(expression)
    | Statement(statement)

    and expression = 
        | Identifier(identifier)

    and statement =
        | Let {
            name: identifier,
            value: expression
        }
        | Return {
            value: expression
        }

    and identifier = {
        identifier: string
    }

    and program = {
        statements: list(node)
};

let token_literal = { fun
    | Program(_) => "program"
    | Expression(_) => "expression"
    | Statement(_) => "statement"
};
