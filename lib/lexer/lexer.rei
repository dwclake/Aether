class t: (~input:string) => { as _;
        pub input: string;

        pub pos: int;
        pub set_pos: int => unit;

        pub read_pos: int;
        pub set_read_pos: int => unit;

        pub ch: char;
        pub set_ch: char => unit;
};

// Lexes from the char currently stored in the lexer, returning the corresponding token and a lexer 
let next_token: ref(t) => Token.t;
