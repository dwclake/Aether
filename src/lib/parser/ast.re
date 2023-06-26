[@deriving (show{with_path: false}, eq)]
type node = 
    | Program(program)
    | Expression(expression)
    | Statement(statement)

    and expression = 
        | Identifier(identifier)
        | Integer(integer)
        | Float(floating)
        | Boolean(bool)
        | Unit
        | If{
            condition: expression,
            consequence: block_statement,
            alternative: option(block_statement)
        }
        | Prefix{
            operator: Token.t,
            value: expression
        } 
        | Infix{
            lhs: expression,
            operator: Token.t,
            rhs: expression
    }

    and statement =
        | Binding {
            kind: Token.t,
            name: identifier,
            value: expression
        }
        | Return {
            value: expression
        }
        | ExpressionStatement {
            value: expression
    }

    and block_statement = list(statement)
    
    and integer = int
    and identifier = string
    and floating = float

    and program = {
        statements: block_statement,
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
        | Boolean(b) => string_of_bool(b)
        | Unit => "()"
        | If(i) => {
            let alternative = switch i.alternative {
                | Some(block) => "else " ++ string(~block)
                | None => ""
            }
            Format.sprintf(
                "if %s {%s} %s",
                string_of_expr(i.condition),
                string(~block=i.consequence),
                alternative
            )
        }
        | Prefix(e) => {
            Format.sprintf(
                "(%s%s)",
                Token.to_string(e.operator),
                string_of_expr(e.value)
            )
        }
        | Infix(e) => {
            Format.sprintf(
                "(%s %s %s)",
                string_of_expr(e.lhs),
                Token.to_string(e.operator),
                string_of_expr(e.rhs)
            )
        }
    };
}

and string(~block: block_statement) = {
    let rec string'(~acc="") = { fun 
        | [] => acc
        | [h,...t] => {
            let literal = switch h {
                | Binding(stmt) => {
                    let value = string_of_expr(stmt.value);
                    Format.sprintf("%s %s = %s;\n", Token.to_string(stmt.kind), stmt.name ,value)
                }
                | Return(stmt) => {
                    let value = string_of_expr(stmt.value);
                    Format.sprintf("return %s;\n", value)
                }
                | ExpressionStatement(stmt) => {
                    let value = string_of_expr(stmt.value);
                    Format.sprintf("%s;\n", value)
                }
            };
            string'(~acc=(acc ++ literal), t)
        }
    };
    string'(block)
};
