```
const pi = 3.14; // A compile time constant binding
let name = "test"; // A immutable binding
let mut greeting = "Hello, world!"; // A mutable binding

let arr = [1, 2, 3, 4]; // An array
arr[0] // => 1

const pos = (3, 5); // A tuple

|x, y| => [x] @ [y] // Anonymous fn definition. @ can be used for appending array, strings

|x| => x ^ x // paren are optional in anonymous fn parameter list, ^ is exponentiation

struct car{ // struct definition with type declarations
    make: string,
    mut model: string,
    color: fn () -> string
}

let myCar = %car{ // An anonymous struct
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

let map = %{"make": "Chevrolet", "model": "Impala"}; // A hashmap with string keys
map["make"] // -> Chevrolet
let map = %{make: "Chevrolet", model: "Impala"}; // A hashmap with polymorphic enums as keys

fn add(a, b) => {
    return a + b;
}

fn add(a, ?b) => { // ? marks optional parameters
    let x = if b { // Options can be cooerced to bools, Some -> true, None -> false
        a + b
    } else {
        a + 0
    };
}

fn add(a, ?b=2) => { // optional parameters can be followed be default values
    let x = if b > 0 {
        a + b
    } else {
        a + 0
    };
}

fn div(a: int, b: int) -> !int => { // ! marks a function which returns either ok(of the type after !) or error
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

fn rec sum(arr, acc) {
    match arr {
        | [] -> acc
        | [h, ...tail] -> {
            sum(tail, acc += h);
        }
    }
}

fn rec fib(x) => {
    match x {
        | a when a > 2 -> 0 // guard clauses follow var name and when
        | 1 -> 1
        | _ -> fib(x - 1) + fib(x - 2)
    }
}

{ // the composition operator >> takes the value of the previous expression 
  // and passes it to the first arg of the function after it
    let nums = 1..100 // Range
        >> collect(...)
        >> map(...) 
        >> filter(...);
}

{   // Operator overloading
    fn ( + ) a, b => {
        a - b
    }
}

{   // S' combinator, evaluates fn1 and fn2, then passes them to fn3
    fn combinator x => fn1(x) <fn3> fn2(x)
    fn findGCD list => max(list) <gcd> mix(list)
}

fn twice ~f, x => { // ~ marks named parameters, paren are optional in fn def
    return f(f(x));
}

fn twice ~f, x => {
    return x >> f >> f;
}

twice ~f:&fib/1, 2; // named parameters must be specified with the name followed by :, paren are optional in fn calls
                  // & needed to create an anonymous fn from a named fn and must be followed by / and the arity ie num of parameters
twice(2, f); // if argument name matches named parameter can just use argument name, named arguments can go in any order

```
