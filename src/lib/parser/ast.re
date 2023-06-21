[@deriving (show, eq)]
type node = 
    | Program(program)
    | Expression(expression)
    | Statement(statement)

    and expression = 
        | Identifier(identifier)
        | Integer(integer)

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

    and integer = {
        value: int
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
                | Let(stmt) => {
                    let value = switch stmt.value {
                        | Identifier(i) => i.identifier
                        | Integer(i) => string_of_int(i.value)
                    };
                    Format.sprintf("let %s = %s;", stmt.name.identifier ,value)
                }
                | Return(stmt) => {
                    let value = switch stmt.value {
                        | Identifier(i) => i.identifier
                        | Integer(i) => string_of_int(i.value)
                    };
                    Format.sprintf("return %s;", value)
                }
                | ExpressionStatement(stmt) => {
                    let value = switch stmt.value {
                        | Identifier(i) => i.identifier
                        | Integer(i) => string_of_int(i.value)
                    };
                    Format.sprintf("%s;", value)
                }
            };
            loop(~acc=(acc ++ literal), t)
        }
    };
    loop(program.statements)
};
