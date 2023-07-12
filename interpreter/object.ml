type t = 
    | Integer of int
    | String of string
    | Float of float
    | Boolean of bool
    | Array of t array
    | Function of func
    | Unit

and func = 
    { parameters: Ast.identifier list
    ; block: Ast.expression
    ; arity: int
    }
;;

let object_type = function
    | Integer _ -> "int"
    | String _ -> "str"
    | Float _ -> "float"
    | Boolean _ -> "bool"
    | Array _ -> "array"
    | Function _ -> "fn"
    | Unit -> "unit"
;;

let inspect = function
    | Integer i -> string_of_int i
    | String s -> s
    | Float f -> string_of_float f
    | Boolean b -> string_of_bool b
    | Unit -> "()"
    | _ -> "todo!"

