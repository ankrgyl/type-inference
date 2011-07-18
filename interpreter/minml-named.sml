(* An explicitly named untyped language and a structure to convert it
 * to De Bruijn terms. Everything is packed into one file because we
 * get rid of it as quickly as possible. As it turns out, it is 
 * very unpleasant to parse directly to De Bruijn terms. *)

structure MinMLNamedUntyped =
struct
  type var = string

  datatype expr =
           EVar of var
         | EApp of expr * expr
         | ELam of var * expr
         | EFix of var * expr
         | ELet of var * expr * expr
         | EUnit
         | EPair of expr * expr
         | EFst of expr
         | ESnd of expr
         | EInl of expr
         | EInr of expr
         | ECase of expr * var * expr * var * expr 
         | ERollList of expr
         | EUnrollList of expr
end

signature DEBRUIJNIFY =
sig
  exception UnboundVariable of string

  (* Convert a named expression to a nameless one under a given
   * context. If the expression references free variables not in
   * the context, raise Malformed. *)
  val removeNames : MinMLNamedUntyped.var Context.context ->
                    MinMLNamedUntyped.expr ->
                    MinMLUntyped.expr
end

structure DeBruijnify :> DEBRUIJNIFY =
struct
  exception UnboundVariable of string

  structure L = MinMLNamedUntyped
  structure B = MinMLUntyped
  structure C = Context

  fun removeNames G e =
      case e of
          L.EVar x =>
          (case C.search (fn (_, y) => x = y) G of
               SOME (i, _) => B.EVar (i, x)
             | NONE => raise UnboundVariable x)
        | L.EApp (e1, e2) => B.EApp (removeNames G e1, removeNames G e2)
        | L.ELam (x, e) => B.ELam (removeNames (C.bind G x) e)
        | L.EFix (x, e) => B.EFix (removeNames (C.bind G x) e)
        | L.ELet (x, e1, e2) => 
          B.ELet (removeNames G e1, removeNames (C.bind G x) e2)
        | L.EUnit => B.EUnit
        | L.EPair (e1, e2) => B.EPair (removeNames G e1, removeNames G e2)
        | L.EFst (e') => B.EFst (removeNames G e')
        | L.ESnd (e') => B.ESnd (removeNames G e')
        | L.EInl (e') => B.EInl (removeNames G e')
        | L.EInr (e') => B.EInr (removeNames G e')
        | L.ECase (e', xl, el, xr, er) => 
          B.ECase (removeNames G e', removeNames (C.bind G xl) el, 
          removeNames (C.bind G xr) er)
        | L.ERollList e => B.ERollList (removeNames G e)
        | L.EUnrollList e => B.EUnrollList (removeNames G e)
end
