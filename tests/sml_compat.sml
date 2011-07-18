(* Direct compatibility functions. *)
fun fst (x, _) = x
fun snd (_, y) = y
datatype ('a, 'b) sum = inl of 'a | inr of 'b
datatype 'a mlist = roll of (unit, 'a * 'a mlist) sum
fun unroll (roll x) = x
