type ('env, 'a) t = Reader of ('env -> 'a Lazy.t) [@@unboxed]

let run (Reader f) r = f r
let pure a = Reader (Fun.const (Lazy.force_val a))
let bind (Reader m) k = Reader (fun r -> run (k (m r)) r)
