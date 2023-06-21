[@deriving (show{with_path: false}, eq)]
type node = 
    | Program(program)
    | Expression(expression)
    | Statement(statement)

    and expression = 
        | Identifier(identifier)
        | Integer(integer)
        | Float(floating)
        | Prefix{
            operator: Token.t,
            value: expression
        }

    and statement =
        | Let {
            name: identifier,
            value: expression
        }
        | Const {
            name: identifier,
            value: expression
        }
        | Return {
            value: expression
        }
        | ExpressionStatement {
            value: expression
        }
    
    and integer = int
    and identifier = string
    and floating = float

    and program = {
        statements: list(statement),
        errors: list(string)
};

let token_literal = { fun
    | Program(_) => "program"
    | Expression(_) => "expression"
    | Statement(_) => "statement"
};

let rec string_of_expr(expr: expression) = {
    switch expr {
        | Identifier(i) => i
        | Integer(i) => string_of_int(i)
        | Float(f) => string_of_float(f)
        | Prefix(e) => {
            Format.sprintf(
                "%s%s",
                Token.to_string(e.operator),
                string_of_expr(e.value)
            )
        }
    };
}

let string(~program: program) = {
    let rec loop(~acc="") = { fun 
        | [] => acc
        | [h,...t] => {
            let literal = switch h {
                | Let(stmt) => {
                    let value = string_of_expr(stmt.value);
                    Format.sprintf("let %s = %s;", stmt.name ,value)
                }
                | Const(stmt) => {
                    let value = string_of_expr(stmt.value);
                    Format.sprintf("let %s = %s;", stmt.name ,value)
                }
                | Return(stmt) => {
                    let value = string_of_expr(stmt.value);
                    Format.sprintf("return %s;", value)
                }
                | ExpressionStatement(stmt) => {
                    let value = string_of_expr(stmt.value);
                    Format.sprintf("%s;", value)
                }
            };
            loop(~acc=(acc ++ literal), t)
        }
    };
    loop(program.statements)
};
