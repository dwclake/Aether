type node =
    | Program of program
    | Statement of statement
    [@@deriving show { with_path = false }, eq]

    and expression =
        | Unit
        | Identifier of identifier
        | Integer of integer
        | Float of floating
        | Boolean of bool
        | Block of block
        | If of {
            condition: expression;
            consequence: expression;
            alternative: expression option;
        }
        | Fn of {
            name: identifier;
            parameter_list: identifier list;
            block: expression;
            arity: int;
        }
        | AnonFn of {
            parameter_list: identifier list;
            block: expression;
            arity: int;
        }
        | Prefix of {
            operator: Token.t;
            value: expression;
        }
        | Infix of {
            lhs: expression;
            operator: Token.t;
            rhs: expression;
        }

    and statement =
        | Binding of { 
            kind: Token.t;
            name: identifier; 
            value: expression
        }
        | Return of {value: expression}
        | Expression of {value: expression}

    and block = statement list
    and integer = int
    and identifier = string
    and floating = float

    and program = { 
        statements: statement list;
        errors: string list
};;

let token_literal = function
    | Program _ -> "program"
    | Statement _ -> "statement"
;;

let rec string ~(block) =
    let rec loop ?(acc="") = function
        | [] -> acc
        | h::t -> begin
            let literal = begin match h with
                | Binding stmt ->
                    let value = string_of_expr stmt.value in
                    Format.sprintf "%s %s = %s;" (Token.to_string stmt.kind) stmt.name value
                | Return stmt ->
                    let value = string_of_expr stmt.value in
                    Format.sprintf "return %s;" value
                | Expression stmt ->
                    let value = string_of_expr stmt.value in
                    Format.sprintf "%s;" value
            end in
            loop ~acc:(acc ^ literal) t
        end
    in
    loop block

and string_of_expr = function
    | Unit -> "()"
    | Identifier i -> i
    | Integer i -> string_of_int i
    | Float f -> string_of_float f
    | Boolean b -> string_of_bool b
    | Block block -> string ~block
    | If i -> 
        let alternative = match i.alternative with
            | Some block -> " else {\n\t" ^ string_of_expr block ^ "\n}"
            | None -> ""
        in
        Format.sprintf 
            "if %s {\n\t%s\n}%s"
            (string_of_expr i.condition)
            (string_of_expr i.consequence)
            alternative
    | Fn f -> 
        f.name ^ Format.sprintf
            "{%s -> %s}/%d"
            (Core.List.fold f.parameter_list ~init:"" ~f:(fun x acc -> x ^ ", " ^ acc))
            (string_of_expr f.block)
            f.arity
    | AnonFn f -> 
        Format.sprintf
            "{%s -> %s}/%d"
            (Core.List.fold f.parameter_list ~init:"" ~f:(fun x acc -> x ^ ", " ^ acc))
            (string_of_expr f.block)
            f.arity
    | Prefix e ->
        Format.sprintf
            "(%s%s)"
            (Token.to_string e.operator)
            @@ string_of_expr e.value
    | Infix e ->
        Format.sprintf
            "(%s %s %s)"
            (string_of_expr e.lhs)
            (Token.to_string e.operator)
            @@ string_of_expr e.rhs
;;
