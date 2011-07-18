local
  structure L = MinMLTyped
in

signature INFERENCE =
sig
  exception TypeError
  exception Occurs of L.evar * L.typ
  exception Unify of L.typ * L.typ

  (* occurs ev t: return whether ev occurs in t *)
  val occurs : L.evar -> L.typ -> bool

  (* unify t1 t2: perform unification of t1 and t2 *)
  val unify : L.typ -> L.typ -> unit

  (* specialize t e: perform type specialization on t and e
   * (eliminating foralls in t and performing type application on e) *)
  val specialize : L.typ -> L.expr -> L.typ * L.expr

  (* generalize G (t, e): generalize t under the context G *)
  val generalize : L.typ Context.context -> L.typ * L.expr 
                   -> L.typ * L.expr

  (* infer G e: preform type inference and term elaboration on
   * e and return the typed term and type. *)
  val infer : L.typ Context.context -> MinMLUntyped.expr ->
              (L.typ * L.expr)

end

end
