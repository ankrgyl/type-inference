signature TYPECHECKER =
sig
  exception TypeError of string

  (* The same context needs to get used for type and term variables,
   * so we have a datatype to distinguish them. Type bindings don't
   * carry information. *)
  datatype binding = TypeBind | TermBind of MinMLTyped.typ

  (* Input should be fully simplified. *)

  (* typeWellFormed G t: if t is well formed under G, return (),
   * otherwise raise TypeError. *)
  val typeWellFormed : binding Context.context -> MinMLTyped.typ -> unit

  (* typecheck G e: typecheck the expression e under the context G.
   * That is, if G |- e : t, return t. If e is not well typed, raise
   * TypeError. *)
  val typecheck : binding Context.context -> MinMLTyped.expr ->
                  MinMLTyped.typ

end
