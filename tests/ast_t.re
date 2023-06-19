open Aether;
open Alcotest;

let test_string() = {
    let program: Ast.program = {
        statements: [
            Ast.Let{
                name: {identifier: "myVar"},
                value: Ast.Identifier{identifier: "anotherVar"}
            },
        ],
        errors: []
    }; 

    if (Ast.string(~program) != "let myVar = anotherVar;") {
        failf("program.string() wrong. got=%s",
           Ast.string(~program)
        );
    }
}
