structure Inference : INFERENCE =
struct
  structure L = MinMLTyped
  structure M = MinMLUntyped
  structure U = TypedUtil
  structure C = Context
  open MinMLTyped

  exception TypeError
  exception Occurs of MinMLTyped.evar * MinMLTyped.typ
  exception Unify of MinMLTyped.typ * MinMLTyped.typ

  val fail = fn _ => raise Fail "unimplemented"

  (* we should never have to check whether a unified type occurs in another. this is the
   * responsibility of the calling code *)
  fun occurs (ref (L.Unified _)) _ = raise Fail "Should never see unified type in occurs check"

    (* a free variable occurs in another if they are equal *)
    | occurs (ref (L.Free x)) (L.TEVar (ref (L.Free y))) = (x = y)

    (* a free variable does not occur in unit *)
    | occurs free_var L.TUnit = false

    (* otherwise, check recursively *)
    | occurs free_var (L.TArrow (t1, t2)) = (occurs free_var t1) orelse (occurs free_var t2)
    | occurs free_var (L.TProd (t1, t2)) = (occurs free_var t1) orelse (occurs free_var t2)
    | occurs free_var (L.TSum (t1, t2)) = (occurs free_var t1) orelse (occurs free_var t2)
    | occurs free_var (L.TList t) = occurs free_var t
    | occurs free_var _ = raise Fail "trying to unify free var with something invalid"


  fun unify (L.TEVar (xref as ref (L.Free x))) (L.TEVar (yref as ref (L.Free y))) =
    (* if they are the same variable, they are already unified. otherwise, make one point
     * to the other *)
    if (x = y) then 
      ()
    else 
      xref := L.Unified (L.TEVar yref)
      
    (* to unify a free variable with some arbitrary type, simplify the type, perform the occurs check,
     * and then (if it succeeds) set the free variable equal to the simplified type *)
    | unify (L.TEVar (xref as ref (L.Free x))) t = 
    let
      val simple_t = U.fullySimplify t
    in
      case simple_t of (L.TEVar (yref as (ref (L.Free y)))) => unify (L.TEVar xref) (L.TEVar yref)
                     | _ => (if occurs xref simple_t then raise Occurs (xref, simple_t)
                             else xref := L.Unified simple_t)
    end
    | unify t (L.TEVar (xref as ref (L.Free x))) = unify (L.TEVar xref) t

    (* if both are references, then unify recursively *)
    | unify (L.TEVar (t1ref as (ref (L.Unified t1)))) t2 = unify t1 t2
    | unify t2 (L.TEVar (t1ref as (ref (L.Unified t1)))) = unify (L.TEVar t1ref) t2

    (* base case- unit unifies to itself *)
    | unify L.TUnit L.TUnit = ()

    (* unify recursively if both represent the same parent type *)
    | unify (L.TArrow (t1, t2)) (L.TArrow (t1', t2')) = (unify t1 t1'; unify t2 t2')
    | unify (L.TProd (t1, t2)) (L.TProd (t1', t2')) =  (unify t1 t1'; unify t2 t2')
    | unify (L.TSum (t1, t2)) (L.TSum (t1', t2')) = (unify t1 t1'; unify t2 t2')
    | unify (L.TList t) (L.TList t') = unify t t'

    (* otherwise, they must be different types, and we cannot unify them *)
    | unify t t' = raise Unify (t,t')


  (* recursively replaces each forall with a type application *)
  fun specialize (L.TForall t) e =
  let
    val a = U.fresh ()
    val t' = U.typeSubstTop a t
  in
    specialize t' (L.EPapp (e, a))
  end
    | specialize t e = (t,e)


  (* the world's slowest set implementation *)
  fun addToSet nil s = [s]
    | addToSet ((xref as ref (L.Free x)) :: L) (sref as ref (L.Free s)) =
      if (x = s orelse xref = sref) then (xref :: L) else xref :: (addToSet L sref)
    | addToSet _ _ = (print "invalid case in set addition\n"; raise Fail "invalid case in set adddition")

  fun unionSet nil nil = nil
    | unionSet nil L = L
    | unionSet (x :: L) L' = unionSet L (addToSet L' x)

  fun setRemove nil s = nil
    | setRemove ((xref as ref (L.Free x)) :: S) (sref as ref (L.Free s)) =
      if (x = s) then S else xref :: (setRemove S sref)
    | setRemove _ _ = (print "invalid case in set remove\n"; raise Fail "invalid case in set remove")

  fun setMinus nil nil = nil
    | setMinus nil S = S
    | setMinus S nil = S
    | setMinus S (x :: S') = setMinus (setRemove S x) S'

  (* performs a depth first search through a type, returning a "set" of free bind refs *)
  fun findFreeTypes (L.TVar _) = nil
    | findFreeTypes (L.TForall t) = findFreeTypes t
    | findFreeTypes (L.TArrow (t1, t2)) = unionSet (findFreeTypes t1) (findFreeTypes t2)
    | findFreeTypes L.TUnit = nil
    | findFreeTypes (L.TProd (t1, t2)) = unionSet (findFreeTypes t1) (findFreeTypes t2)
    | findFreeTypes (L.TSum (t1, t2)) = unionSet (findFreeTypes t1) (findFreeTypes t2)
    | findFreeTypes (L.TList t) = findFreeTypes t
    | findFreeTypes (L.TEVar (ref (L.Unified t))) = findFreeTypes t
    | findFreeTypes (L.TEVar (kref as ref (L.Free k))) = [kref]

  (* flattens a context into a list of free bind refs *)
  fun flattenFreeRefs c =
  let
    fun appender (t, L) = unionSet (findFreeTypes t) L
  in
    foldr appender nil (Context.toList c)
  end

  fun replaceFreeT (xref, t) = L.TForall (U.typeEvSubst (L.TVar (0, "_g"))  xref (U.typeShift 1 t))
  fun replaceFreeE (xref, e) = L.EPlam (U.typeTermEvSubst (L.TVar (0, "_g")) xref (U.termShift 1 e))

  (* you can ignore this- i was using it while I debugged. feel free to use it to print the lists
   * of bind refs I generate *)
  fun printList nil = "\n"
    | printList ((xref as (ref (L.Free x))) :: L) = Int.toString (x) ^ ", " ^ (printList L)
    | printList _ = "FUCK"
  fun tListToString nil = "\n"
    | tListToString ((L.TEVar (ref (L.Free k))) :: L) = Int.toString (k) ^ ", " ^ (tListToString L)
    | tListToString (x :: L) = (U.typeToString x) ^ "," ^ (tListToString L)

  fun generalize c (t, e) =
  let
    (* find the free types in the type and the context, and then remove the context ones *)
    val allFree = findFreeTypes t
    val cFree = flattenFreeRefs c
    val newFreeTypes = setMinus allFree cFree

    (* fold the existing type and expression into foralls and big lambdas, respectively *)
    val result_type = foldr replaceFreeT t newFreeTypes
    val result_expression = foldr replaceFreeE e newFreeTypes
  in
    (result_type, result_expression)
  end

  fun infer c M.EUnit = (L.TUnit, L.EUnit)

    | infer c (M.EPair (e1, e2)) =
    let
      val (t1, e1_typed) = infer c e1
      val (t2, e2_typed) = infer c e2
    in
      (L.TProd (t1, t2), L.EPair (e1_typed, e2_typed))
    end
    | infer c (M.EFst e) =
    let
      val (t, e_typed) = infer c e
      val a = U.fresh ()
      val b = U.fresh ()
      val _ = unify t (L.TProd (a,b))
    in
      (a, L.EFst e_typed)
    end
    | infer c (M.ESnd e) =
    let
      val (t, e_typed) = infer c e
      val a = U.fresh ()
      val b = U.fresh ()
      val _ = unify t (L.TProd (a,b))
    in
      (b, L.ESnd e_typed)
    end

    | infer c (M.EInl e) =
    let
      val a = U.fresh ()
      val (t, e_typed) = infer c e
    in
      (L.TSum (t, a), L.EInl (e_typed, L.TSum(t, a)))
    end
    | infer c (M.EInr e) =
    let
      val a = U.fresh ()
      val (t, e_typed) = infer c e
    in
      (L.TSum (a, t), L.EInr (e_typed, L.TSum(a, t)))
    end
    | infer c (M.ECase (e, e1, e2)) =
    let
      val (t, e_typed) = infer c e
      val a = U.fresh ()
      val b = U.fresh ()
      val (t1, e1_typed) = infer (C.bind c a) e1
      val (t2, e2_typed) = infer (C.bind c b) e2
      val _ = unify t (L.TSum (a, b))
      val _ = unify t1 t2
    in
      (t1, L.ECase (e_typed, e1_typed, e2_typed))
    end

    | infer c (M.ERollList e) =
    let
      val (t, e_typed) = infer c e
      val a = U.fresh()
      val _ = unify t (L.TSum (L.TUnit, L.TProd (a, L.TList a)))
    in
      (L.TList a, L.ERollList e_typed)
    end

    | infer c (M.EUnrollList e) =
    let
      val (t, e_typed) = infer c e
      val a = U.fresh ()
      val _ = unify t (L.TList a)
    in
      (L.TSum (L.TUnit, L.TProd (a, L.TList a)), L.EUnrollList e_typed)
    end

    | infer c (M.EApp (e1, e2)) =
    let
      val (t1, e1_typed) = infer c e1
      val (t2, e2_typed) = infer c e2
      val a = U.fresh ()
      val _ = unify t1 (L.TArrow (t2, a))
    in
      (a, L.EApp (e1_typed, e2_typed))
    end

    | infer c (M.ELam e) =
    let
      val a = U.fresh ()
      val (t, e_typed) = infer (C.bind c a) e
    in
      (L.TArrow (a, t), L.ELam (a, e_typed))
    end

    | infer c (M.EFix e) =
    let
      val a = U.fresh()
      val (t, e_typed) = infer (C.bind c a) e
      val _ = unify t a
    in
      (t, L.EFix (t, e_typed))
    end

    | infer c (M.EVar (k, s)) = (
      case (C.lookup c k) of (SOME t) => specialize t (L.EVar (k, s))
                           | _ => raise TypeError )

    | infer c (M.ELet (e1, e2)) =
    let
      val (t1, e1_typed) = infer c e1
      val (t1_g, e1_typed_g) = generalize c (t1, e1_typed)
      val (t2, e2_typed) = infer (C.bind c t1_g) e2
    in
      (t2, L.ELet (e1_typed_g, e2_typed))
    end

end
