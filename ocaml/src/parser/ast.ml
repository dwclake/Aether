type node =
    | PROGRAM of program
    | EXPRESSION of expression
    | STATEMENT of statement
    [@@deriving show { with_path = false }, eq]

    and expression =
        | IDENTIFIER of identifier

    and statement =
        | LET of { 
            name: identifier; 
            value: expression
        }
        | RETURN of {
            value: expression
        }

    and identifier = { 
        identifier: string 
    }      
    and program = { 
        statements: node list 
};;

let token_literal = function
    | PROGRAM _ -> "program"
    | EXPRESSION _ -> "expression"
    | STATEMENT _ -> "statement"
;;
