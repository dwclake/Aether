type node =
    | PROGRAM of program
    | EXPRESSION of expression
    | STATEMENT of statement
[@@deriving show { with_path = false }, eq]

and expression =
    | IDENTIFIER of identifier

and statement =
    | LET of { 
        name : identifier; 
        value : expression
    }

and identifier = { 
    identifier : string 
}
and program = { 
    statements : statement list 
};;

let token_literal = function
    | PROGRAM _ -> "program"
    | EXPRESSION e -> 
        begin match e with
            | IDENTIFIER i -> i.identifier 
        end
    | STATEMENT s ->
        begin match s with
            | LET n -> n.name.identifier
        end
;;
