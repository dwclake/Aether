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
    
    check(Alcotest.string, "1", "let myVar = anotherVar;", Ast.string(~program))
}
