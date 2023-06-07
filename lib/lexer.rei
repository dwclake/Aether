type t

let create: (~input:string) => t;
let next_token: t => (t, Token.t);
