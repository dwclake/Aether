open Aether;
open Alcotest;

let test_string() = {
    let program: Ast.program = {
        statements: [
            Ast.Binding{
                kind: Token.Let,
                name: "myVar",
                value: Ast.Identifier("anotherVar")
            },
        ],
        errors: []
    }; 
    
    check(Alcotest.string, "1", "let myVar = anotherVar;", Ast.string(~program))
}
