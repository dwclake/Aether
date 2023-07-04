```
const pi = 3.14; // A compile time constant binding
let name = "test"; // A immutable binding
let mut greeting = "Hello, world!"; // A mutable binding

let arr = [1, 2, 3, 4]; // An array
arr[0] // => 1

const pos = (3, 5); // A tuple

fn (x, y) => [x] @ [y] // Anonymous fn definition. @ can be used for appending array, strings, and fn composition

fn x => x ^ x // paren are optional in anonymous fn parameter list, ^ is exponentiation

let myCar = { as _; // An anonymous struct, the identifier following as is the name of the "self" pointer, _ sets it as unused
    make: "Mazda",
    mut model: "RX-7",
    color: fn -> return "red";
}

let make = myCar.make; // member access

enum traffic_light = { // enumeration definition, enum variants can hold data
	red(int),
	yellow,
	green
}

const x = traffic_light.red(int);

let y = :red; // polymorphic enum start with :

struct car { // struct definition with type declarations
    make: string,
    mut model: string,
    color: fn () -> string
}

let map = %{"make": "Chevrolet", "model": "Impala"}; // A hashmap with string keys
map["make"] // -> Chevrolet
let map = %{make: "Chevrolet", model: "Impala"}; // A hashmap with polymorphic enums as keys

let add(a, b) => {
    return a + b;
}

let add(a, ?b) => { // ? marks optional parameters
    let x = if b { // Options can be cooerced to bools, Some -> true, None -> false
        a + b
    } else {
        a + 0
    };
}

let add(a, ?b=2) => { // optional parameters can be followed be default values
    let x = if b > 0 {
        a + b
    } else {
        a + 0
    };
}

let div(a: int, b: int) -> !int => { // ! marks a function which returns either ok(of the type after !) or error
    if b == 0 {
        return err("cannot divide by zero")
    } else {
        ok(a / b)
    }
}

match arr {
    | [] -> *code*
    | [e, ...rest] -> *code*
    | _ -> {
        "Hello" @ " world!" // @ can be used for string concatenation
    }
}

let rec sum(arr, acc) {
    match arr {
        | [] -> acc
        | [h, ...tail] -> {
            sum(tail, acc += h);
        }
    }
}

let rec fib(x) => {
    match x {
        | a when a > 2 -> 0 // guard clauses follow var name and when
        | 1 -> 1
        | _ -> fib(x - 1) + fib(x - 2)
    }
}

{ // the composition operator @ takes the value of the previous expression and passes it to the function after it
    let mut nums = 1..100 // Range
        @ collect(...)
        @ map(...) 
        @ filter(...);
}

let twice ~f, x => { // ~ marks named parameters, paren are optional in fn def
    return f(f(x));
}

let twice ~f, x => {
    return x @ f @ f;
}

twice ~f:fib, 2; // named parameters must be specified with the name followed by :, paren are optional in fn calls
twice(2, f); // if argument name matches named parameter can just use argument name, named arguments can go in any order

```
