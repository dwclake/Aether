type t

let create: (~input:string) => t;
let read_char: t => t;
let next_token: t => (t, Token.t);
