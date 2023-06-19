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
        | ExpressionStatement {
            value: expression
        }

    and identifier = {
        identifier: string
    }

    and program = {
        statements: list(statement),
        errors: list(string)
};

let token_literal = { fun
    | Program(_) => "program"
    | Expression(_) => "expression"
    | Statement(_) => "statement"
};

let string(~program: program) = {
    let rec loop(~acc="") = { fun 
        | [] => acc
        | [h,...t] => {
            let literal = switch h {
                | Let(s) => {
                    let value = switch s.value {
                        | Identifier(i) => i.identifier
                    };

                    Format.sprintf(
                        "let %s = %s;",
                        s.name.identifier,
                        value
                    )
                }
                | Return(s) => {
                    let value = switch s.value {
                        | Identifier(i) => i.identifier
                    };

                    Format.sprintf(
                        "return %s;",
                        value
                    )
                }
                | ExpressionStatement(s) => {
                    let value = switch s.value {
                        | Identifier(i) => i.identifier
                    };

                    Format.sprintf(
                        "%s;",
                        value
                    )
                }
            };

            loop(~acc=(acc ++ literal), t)
        }
    };

    loop(program.statements)
};
