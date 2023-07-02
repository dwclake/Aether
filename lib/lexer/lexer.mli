class t: input:string -> 
    object
        val input': string
        method input: string
        
        val mutable pos': int
        method pos: int
        method set_pos: int -> unit
        
        val mutable read_pos': int
        method read_pos: int
        method set_read_pos: int -> unit
        
        val mutable ch': char
        method ch: char
        method set_ch: char -> unit
end

val next_token: t ref -> Token.t
