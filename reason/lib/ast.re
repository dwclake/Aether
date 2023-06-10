type identifier = {
    identifier: string
}

type expression = 
    | IDENTIFIER(identifier)

type statement =
    | LET {
        name: identifier,
        value: expression
    }

type program = {
    statements: list(statement)
}

type node = 
    | PROGRAM(program)
    | EXPRESSION(expression)
    | STATEMENT(statement);

let token_literal = { fun
    | PROGRAM(_) => "program"
    | EXPRESSION(_) => "expression"
    | STATEMENT(_) => "statement"
}
