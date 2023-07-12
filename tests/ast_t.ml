open Briar_i
open Alcotest

let test_string() =
    let program: Ast.program = {
        statements= [Ast.Binding
            { kind= Token.Let
            ; name= Ast.{identifier= "myVar"}
            ; value= Ast.Identifier {identifier= "anotherVar"}
            };
        ];
    } 
    in
    check Alcotest.string "1" "let myVar = anotherVar;\n" @@ Ast.string program.statements
;;
