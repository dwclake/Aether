module Task = Domainslib.Task;

let rec fib = (n) => {
    if (n < 2) {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

let rec fib_par = (pool, n) => {
    if (n > 20) {
        let a = Task.async(pool, ( _ => fib_par(pool, n - 1)));
        let b = Task.async(pool, ( _ => fib_par(pool, n - 2)));

        Task.await(pool, a) + Task.await(pool, b)
    } else {
        fib(n)
    }
}

let num_domains = try (int_of_string(Sys.argv[1])) {
    | _ => 1
};
 
let n = try (int_of_string(Sys.argv[2])) {
    | _ => 1
};

let main () = {
    let pool = Task.setup_pool(~num_domains=(num_domains - 1))();
    let res = Task.run(pool, _ => fib_par(pool, n));

    Task.teardown_pool(pool);
    Stdio.printf("\nfib(%d) = %d\n", n, res);
};

let () = main();
