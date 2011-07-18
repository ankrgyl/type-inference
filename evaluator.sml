structure Evaluator :> EVALUATOR =
struct
  structure L = MinMLTyped
  structure U = TypedUtil

  exception Stuck of MinMLTyped.expr

  fun eval (L.EVar (k,s)) = L.EVar (k,s)
    | eval (L.EApp (e1, e2)) =
    let
      val v1 = eval e1
      val v2 = eval e2
    in
      case v1 of (L.ELam (t, v1')) => eval (U.termSubstTop v2 v1')
               | _ => raise Stuck (L.EApp (v1, v2))
    end
    | eval (L.ELam (t, e)) = L.ELam (t,e)
    | eval (L.EPlam e) = L.EPlam e
    | eval (L.EPapp (e, t)) =
    let
      val e_final = eval e
    in
      case e_final of (L.EPlam e') => eval (U.typeTermSubstTop t e')
                    | _ => raise Stuck (L.EPapp (e_final, t))
    end
    | eval (L.EFix (t, e)) = eval (U.termSubstTop (L.EFix (t, e)) e)
    | eval (L.ELet (e1, e2)) =
    let
      val v1 = eval e1
    in
      eval (U.termSubstTop v1 e2)
    end
    | eval L.EUnit = L.EUnit
    | eval (L.EPair (e1, e2)) = L.EPair (eval e1, eval e2)
    | eval (L.EFst e) =
    let
      val v = eval e
    in
      case v of (L.EPair (v1, v2)) => v1
              | _ => raise Stuck (L.EFst v)
    end
    | eval (L.ESnd e) =
    let
      val v = eval e
    in
      case v of (L.EPair (v1, v2)) => v2
              | _ => raise Stuck (L.ESnd v)
    end
    | eval (L.EInl (e, t)) = L.EInl (eval e, t)
    | eval (L.EInr (e, t)) = L.EInr (eval e, t)
    | eval (L.ECase (e, e1, e2)) =
    let
      val cond = eval e
    in
      case cond of (L.EInl (e', t)) => eval (U.termSubstTop e' e1)
                 | (L.EInr (e', t)) => eval (U.termSubstTop e' e2)
                 | _ => raise Stuck (L.ECase (cond, e1, e2))
    end
    | eval (L.ERollList e) = L.ERollList (eval e)
    | eval (L.EUnrollList e) = 
    let
      val v = eval e
    in
      case v of (L.ERollList v') => v'
              | _  => raise Stuck (L.EUnrollList v)
    end
end
