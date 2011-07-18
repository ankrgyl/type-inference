(* A context for use with nameless terms. *)
signature CONTEXT =
sig
  type 'a context

  val empty : 'a context

  (* Bind something to a new variable in a context of nameless terms.
   * The newly bound piece of information will be at index 0 and all
   * existing variables in the context have their indexes shifted up by
   * 1. *)
  val bind : 'a context -> 'a -> 'a context

  (* Look up the data associated with a variable in the context. *)
  val lookup : 'a context -> int -> 'a option

  (* Search through the context, calling a predicate with each
   * variable/data pair. If the predicate returns true for some (v, x)
   * returns SOME (v, x). If the predicate never returns true,
   * returns NONE. *)
  val search : ((int * 'a) -> bool) -> 'a context -> (int * 'a) option

  (* Return the list of all data in the context, in ascending order
   * by variable number. *)
  val toList : 'a context -> 'a list
end
