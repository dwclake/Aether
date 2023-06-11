type node =
  | PROGRAM of program
  | EXPRESSION of expression
  | STATEMENT of statement

and expression =
  | IDENTIFIER of identifier

and statement =
  | LET of { 
      name : identifier; 
      value : expression
    }
[@@deriving show { with_path = false }]

and identifier = { 
    identifier : string 
}

and program = { 
    statements : statement list 
}

let token_literal = function
    | PROGRAM _ -> "program"
    | EXPRESSION e -> (match e with
        | IDENTIFIER i -> i.identifier
    )
    | STATEMENT s -> (match s with
        | LET n -> n.name.identifier
    )
;;
