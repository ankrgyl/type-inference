local
    structure L = MinMLTyped
in

signature TYPED_UTIL =
sig
  (* Generates a fresh type unification variable. *)
  val fresh : unit -> L.typ

  (* simplify t: simplify the type t, doing path compression *)
  val simplify : L.typ -> L.typ
  (* fullySimplify t: reduces t to a fully simple form with no subexpressions
                       that are of the form "TEVar (ref (SOME t))" *)
  val fullySimplify : L.typ -> L.typ
  (* simplifyTerm e: fully simplifies all types appearing in e *)
  val simplifyTerm : L.expr -> L.expr

  (* Simple utility functions for use with the maps that act as noops. *)
  val tvarId : int * int * string -> L.typ
  val tevarId : int * L.evar -> L.typ
  val evarId : int * int * string -> L.expr
  val typId : int * L.typ -> L.typ

  (* typeShift d t: increase the value of all free variables in t by d *)
  val typeShift : int -> L.typ -> L.typ
  (* subst t a t': return [t/a]t' *)
  val typeSubst : L.typ -> int -> L.typ -> L.typ
  (* typeSubstTop t t': substitutes t for the topmost variable in t',
     properly accounting for free variables *)
  val typeSubstTop : L.typ -> L.typ -> L.typ

  (* termShift d e: increase the value of all free variables in e by d *)
  val termShift : int -> L.expr -> L.expr
  (* subst e x e': return [e/x]e' *)
  val termSubst : L.expr -> int -> L.expr -> L.expr
  (* termSubstTop e e': substitutes e for the topmost variable in e',
     properly accounting for free variables *)
  val termSubstTop : L.expr -> L.expr -> L.expr

  (* typeEvSubst t ev t': return [t/ev]t' *)
  val typeEvSubst : L.typ -> L.evar -> L.typ -> L.typ
  (* typeTermEvSubst t ev e: return [t/ev]e *)
  val typeTermEvSubst : L.typ -> L.evar -> L.expr -> L.expr


  (* typeTermSubst t a e: return [t/a]e *)
  val typeTermSubst : L.typ -> int -> L.expr -> L.expr
  (* typeTermSubstTop e e': substitutes t for the topmost variable in e,
     properly accounting for free variables *)
  val typeTermSubstTop : L.typ -> L.expr -> L.expr

  (* performs equality check on types *)
  val typEq : L.typ -> L.typ -> bool

  (* performs equality check on terms *)
  val termEq : L.expr -> L.expr -> bool
  
  (* returns a string representation of a type *)
  val typeToString : L.typ -> string
  (* returns a string representation of a type in prenex
   * form in SML/NJ style *)
  val typeToStringNice : L.typ -> string
  (* returns a string representation of a term *)
  val termToString : L.expr -> string
end

end
