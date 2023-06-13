```
bind pi = 3.14; // A immutable binding
let name = "test"; // A mutable binding

let arr = [1, 2, 3, 4]; // An array
arr[0] // -> 1

bind pos = (3, 5); // A tuple

let map = {"make": "Chevrolet", "model": "Impala"}; // A hashmap
map["make"] // -> Chevrolet

fn add(a, b) {
    return a + b;
};

fn add(a, ?b) { // ? marks optional parameters
    let x = if b.is_some() {
        a + b
    } else {
        a + 0
    };
};

fn div(a: int, b: int) -> !int { // ! marks a function which returns either ok(of the type after !) or error
    if b == 0 {
        return err("cannot divide by zero")
    } else {
        ok(a / b)
    }
}

match arr {
    | [] -> *code*
    | [e, ...rest] -> *code*
    | _ -> {*code*}
};

fn rec fib(x) {
    match x {
        | a: a > 2 => 0
        | 1 => 1
        | _ => fib(x - 1) + fib(x - 2)
    }
};

fn twice(f, x) {
    return f(f(x));
}

twice(fib, 2);

```