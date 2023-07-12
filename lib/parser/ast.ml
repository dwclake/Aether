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
        | If of 
            { condition: expression
            ; consequence: expression
            ; alternative: expression option
            }
        | Fn of 
            { name: identifier
            ; parameters: identifier list
            ; block: expression
            ; arity: int
            }
        | AnonFn of 
            { parameters: identifier list
            ; block: expression
            ; arity: int
            }
        | FnCall of 
            { fn: expression
            ; arguments: expression list
            }
        | Prefix of 
            { operator: Token.t
            ; value: expression
            }
        | Infix of 
            { lhs: expression
            ; operator: Token.t
            ; rhs: expression
            }

    and statement =
        | Binding of 
            { kind: Token.t
            ; name: identifier 
            ; value: expression
            }
        | Return of {value: expression}
        | Expression of {value: expression}

    and block = statement list
    and identifier = {identifier: string}
    and integer = int
    and floating = float

    and program = 
        { statements: statement list
        }
;;

let token_literal = function
    | Program _ -> "program"
    | Statement _ -> "statement"
;;

let rec string block =
    let rec string' ?(acc="") = function
        | [] -> acc
        | h::t -> begin
            let literal = begin match h with
                | Binding stmt ->
                    let value = string_of_expr stmt.value in
                    Format.sprintf "%s %s = %s;" (Token.to_string stmt.kind) stmt.name.identifier value
                | Return stmt ->
                    let value = string_of_expr stmt.value in
                    Format.sprintf "return %s;" value
                | Expression stmt ->
                    let value = string_of_expr stmt.value in
                    Format.sprintf "%s;" value
            end in
            string' ~acc:(acc ^ literal) t
        end
    in
    string' block

and string_of_expr = function
    | Unit -> "()"
    | Identifier {identifier} -> identifier
    | Integer i -> string_of_int i
    | Float f -> string_of_float f
    | Boolean b -> string_of_bool b
    | Block block -> string block
    | If i -> 
        let alternative = match i.alternative with
            | Some block -> " else {\n    " ^ string_of_expr block ^ "\n}"
            | None -> ""
        in
        Format.sprintf 
            "if %s {\n    %s\n}%s"
            (string_of_expr i.condition)
            (string_of_expr i.consequence)
            alternative
    | Fn f -> 
        let params_to_string = match f.parameters with
            | [] -> (fun acc _ -> acc ^ "")
            | [y] -> (fun acc x -> y.identifier ^ acc ^ x.identifier)
            | _ -> (fun acc x -> x.identifier ^ ", " ^ acc)
        in
        let params = f.parameters
            |> List.rev
            |> Core.List.fold ~init:"" ~f:params_to_string
            |> String.trim
            |> remove_trailing_comma
        in
        f.name.identifier ^ Format.sprintf
            "%s => {\n\t%s\n}/%d"
            params
            (string_of_expr f.block)
            f.arity
    | AnonFn f -> 
        let params_to_string = match f.parameters with
            | [] -> (fun acc _ -> acc ^ "")
            | [y] -> (fun acc x -> y.identifier ^ acc ^ x.identifier)
            | _ -> (fun acc x -> x.identifier ^ ", " ^ acc)
        in
        let params = f.parameters
            |> List.rev
            |> Core.List.fold ~init:"" ~f:params_to_string
            |> String.trim
            |> remove_trailing_comma
        in
        Format.sprintf
            "|%s| => {\n    %s\n}/%d"
            params
            (string_of_expr f.block)
            f.arity
    | FnCall f ->
        let args_to_string = match f.arguments with
            | [] -> (fun acc _ -> acc ^ "")
            | [y] -> (fun acc x -> acc ^ (string_of_expr y) ^ (string_of_expr x))
            | _ -> (fun acc x -> (string_of_expr x) ^ ", " ^ acc)
        in
        let args = f.arguments
            |> List.rev
            |> Core.List.fold ~init:"" ~f:args_to_string
            |> String.trim
            |> remove_trailing_comma
        in
        Format.sprintf
            "%s(%s)"
            (string_of_expr f.fn)
            args
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

and remove_trailing_comma s =
    Core.String.filteri s ~f:(fun x _ -> match x with 
        | i when i == (String.length s) - 1 -> false
        | _ -> true
    )
;;
