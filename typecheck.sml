structure Typechecker : TYPECHECKER =
struct
  exception TypeError of string

  structure L = MinMLTyped
  structure C = Context
  structure U = TypedUtil
  
  fun typEq t1 t2 =
      case (t1, t2) of
          (L.TVar (k, _), L.TVar (k', _)) => k = k'
        | (L.TEVar (ref (L.Free k)), L.TEVar (ref (L.Free k'))) => k = k'
        | (L.TForall t, L.TForall t') => typEq t t'
        | (L.TUnit, L.TUnit) => true
        | (L.TArrow (t1, t2), L.TArrow (t1', t2')) => typEq t1 t1' andalso typEq t2 t2'
        | (L.TProd (t1, t2), L.TProd (t1', t2')) => typEq t1 t1' andalso typEq t2 t2'
        | (L.TSum (t1, t2), L.TSum (t1', t2')) => typEq t1 t1' andalso typEq t2 t2'
        | (L.TList t, L.TList t') => typEq t t'
        | _ => false

  datatype binding = TypeBind | TermBind of MinMLTyped.typ

  (* Does a lookup, properly shifting a binding if need be. *)
  fun lookup G x =
      case C.lookup G x of
          SOME TypeBind => SOME TypeBind
        | SOME (TermBind t) => SOME (TermBind (U.typeShift (x+1) t))
        | NONE => NONE

  (* typeWellFormed G t: if t is well formed under G, return (),
   * otherwise raise TypeError. *)
  fun typeWellFormed G t =
      let val tc = typeWellFormed G
      in
      case t of
          L.TVar (x, _) =>
          (case lookup G x of SOME TypeBind => ()
                            | _ => raise TypeError "unbound type variable")
        | L.TEVar _ => ()
        | L.TUnit => ()
        | L.TArrow (t1, t2) => (tc t1; tc t2)
        | L.TProd (t1, t2) => (tc t1; tc t2)
        | L.TSum (t1, t2) => (tc t1; tc t2)
        | L.TList t => tc t
        | L.TForall t => typeWellFormed (C.bind G TypeBind) t
      end

  fun forceEq t1 t2 = if typEq t1 t2 then ()
                      else raise TypeError (U.typeToString t1 ^ " <> " ^
                                            U.typeToString t2)

  fun typecheck G e =
      let val twf = typeWellFormed G
          val tc = typecheck G
          fun tcbind t = U.typeShift (~1) o typecheck (C.bind G (TermBind t))
      in
      case e of
          L.EUnit => L.TUnit
        | L.EVar (x, _) => (case lookup G x of SOME (TermBind t) => t
                                             | _ => raise TypeError "unbound term variable")
        | L.EApp (e1, e2) =>
          (case tc e1 of L.TArrow (t1, t2) => (forceEq t1 (tc e2); t2)
                       | _ => raise TypeError "application to a non function type")
        | L.ELam (t, e) => L.TArrow (t, tcbind t e)
        | L.EPlam e => L.TForall (typecheck (C.bind G TypeBind) e)
        | L.EPapp (e, t) =>
          (twf t;
           case tc e of L.TForall t' => U.typeSubstTop t t'
                      | t' => raise TypeError ("type app to a non forall: "
                                               ^ U.typeToString t'))
        | L.EFix (t, e) =>
          let val () = twf t
              val t' = tcbind t e
              val () = forceEq t t'
          in t end
        | L.ELet (e1, e2) => tcbind (tc e1) e2
        | L.EPair (e1, e2) => L.TProd (tc e1, tc e2)
        | L.EFst e =>
          (case tc e of L.TProd (t1, _) => t1
                      | _ => raise TypeError "projection from non-pair")
        | L.ESnd e =>
          (case tc e of L.TProd (_, t2) => t2
                      | _ => raise TypeError "projection from non-pair")
        | L.EInl (e, t as L.TSum (t1, _)) => (twf t; forceEq (tc e) t1; t)
        | L.EInr (e, t as L.TSum (_, t2)) => (twf t; forceEq (tc e) t2; t)
        | L.ECase (e, e1, e2) =>
          (case tc e of
               L.TSum (t1, t2) =>
               let val t = tcbind t1 e1
                   val t' = tcbind t2 e2
                   val () = forceEq t t'
               in t end
             | _ => raise TypeError "case on non-sum")
        | L.ERollList e =>
          (case tc e of
               L.TSum (L.TUnit, L.TProd (t, L.TList t')) =>
               (forceEq t t'; L.TList t)
             | t => raise TypeError ("invalid roll: " ^ U.typeToString t))
        | L.EUnrollList e =>
          (case tc e of
               L.TList t => L.TSum (L.TUnit, L.TProd (t, L.TList t))
             | _ => raise TypeError "unroll on non-list")
        | _ => raise TypeError "ruh roh."
      end
end
