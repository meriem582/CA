open Ast


let rec exec : config -> config = function
  | ({ contents = Pair(x, y) }, Car :: c, d) -> exec (ref x, c, d)
  | ({ contents = Pair(x, y) }, Cdr :: c, d) -> exec (ref y, c, d)
  | (x, Cons :: c, Val y :: d) -> exec (ref (Pair (y, !x)), c, d)
  | (x, Rplac :: c, Val (Pair (y, z) as u) :: d) -> ref z := !x; exec (ref u, c, d)
  | ({ contents = x }, Push :: c, d) -> exec (ref x, c, Val x :: d)
  | ({ contents = x }, Swap :: c, Val y :: d) -> exec (ref y, c, Val x :: d)
  | (t, Quote v :: c, d) -> exec (ref v, c, d)
  | ({ contents = Pair (Closure (x, y), z) }, App :: c, d) -> exec (ref (Pair (y, z)), x, Code c :: d)
  | ({ contents = Bool b }, Branch (c1, c2) :: c, Val x :: d) -> exec (ref x, (if b then c1 else c2), Code c :: d)
  | ({ contents = Pair (Int m, Int n) }, Opc Add :: c, d) -> exec (ref (Int (m + n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Sub :: c, d) -> exec (ref (Int (m - n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Mult :: c, d) -> exec (ref (Int (m * n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Div :: c, d) -> exec (ref (Int (m / n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Lt :: c, d) -> exec (ref (Bool (m < n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Gt :: c, d) -> exec (ref (Bool (m > n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Eq :: c, d) -> exec (ref (Bool (m = n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Eqeq :: c, d) -> exec (ref (Bool (m == n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Leq :: c, d) -> exec (ref (Bool (m <= n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Geq :: c, d) -> exec (ref (Bool (m >= n)), c, d)
  | ({ contents = x }, Cur c1 :: c, d) -> exec (ref (Closure (c1, x)), c, d)
  | ({ contents = x }, c, Code c' :: d) -> exec (ref x, c', d)
  | config -> config

let exec_code cs = exec (ref Nullvalue, cs, [])
