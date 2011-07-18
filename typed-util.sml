structure TypedUtil :> TYPED_UTIL =
struct
  structure L = MinMLTyped

  (* fresh (): generates a fresh evar *)
  local
      val ctr = ref 0
  in
  fun fresh () = L.TEVar (ref (L.Free (!ctr before ctr := !ctr + 1)))
  end

(*****************************************************************************)
(** You need to implement this                                               *)
(** Note that an implementation of simplification is needed for type pretty  *)
(** printing                                                                 *)
(*****************************************************************************)

  (* A type is in simplified form if it is not of the form:
     TEVar (ref (Unified t)) *)
  (* simplify t: simplify the type t, doing path compression *)
  fun simplify (L.TEVar (tref as ref (L.Unified t))) = let
    val ans = simplify t
  in 
    (tref := L.Unified ans); ans
  end
    | simplify t = t

(*****************************************************************************)
(*****************************************************************************)


  (* Type and term mapping functions *)
  fun typeMap f_var f_evar c t = let
      fun walk c t = let
          val walk0 = walk c
          val walk1 = walk (c+1)
      in
          case simplify t of
              L.TVar (k, x) => f_var (c, k, x)
            | L.TForall e => L.TForall (walk1 e)
            | L.TArrow (t1, t2) => L.TArrow (walk0 t1, walk0 t2)
            | L.TProd (t1, t2) => L.TProd (walk0 t1, walk0 t2)
            | L.TSum (t1, t2) => L.TSum (walk0 t1, walk0 t2)
            | L.TUnit => L.TUnit
            | L.TList t => L.TList (walk0 t)
            | L.TEVar ev => f_evar (c, ev)
      end
  in walk c t end

  fun termMap f_var f_typ c e = let
      fun walk c e = let
          val walk0 = walk c
          val walk1 = walk (c+1)
      in
          case e of
              L.EVar (k, x) => f_var (c, k, x)
            | L.EApp (e1, e2) => L.EApp (walk0 e1, walk0 e2)
            | L.ELam (t, e) => L.ELam (f_typ (c, t), walk1 e)
            | L.EPlam e => L.EPlam (walk1 e)
            | L.EPapp (e, t) => L.EPapp (walk0 e, f_typ (c, t))
            | L.EFix (t, e) => L.EFix (f_typ (c, t), walk1 e)
            | L.ELet (e1, e2) => L.ELet (walk0 e1, walk1 e2)
            | L.EUnit => L.EUnit
            | L.EPair (e1, e2) => L.EPair (walk0 e1, walk0 e2)
            | L.EFst e => L.EFst (walk0 e)
            | L.ESnd e => L.ESnd (walk0 e)
            | L.EInl (e, t) => L.EInl (walk0 e, f_typ (c, t))
            | L.EInr (e, t) => L.EInr (walk0 e, f_typ (c, t))
            | L.ECase (e, e1, e2) => L.ECase (walk0 e, walk1 e1, walk1 e2)
            | L.ERollList e => L.ERollList (walk0 e)
            | L.EUnrollList e => L.EUnrollList (walk0 e)
      end
  in walk c e end

  fun tvarId (_, k, x) = L.TVar (k, x)
  fun tevarId (_, t) = L.TEVar t
  fun evarId (_, k, x) = L.EVar (k, x)
  fun typId (_, t) = t

  (* fullySimplify t: reduces t to a fully simple form with no subexpressions
                      that are of the form "TEVar (ref (SOME t))" *)
  fun fullySimplify t =
      typeMap tvarId (fn (_, ev as (ref (L.Unified t))) => 
                         fullySimplify (simplify (L.TEVar ev))
                       | (_, ev) => L.TEVar ev) 0 t
  (* simplifyTerm e: fully simplifys all types appearing in e *)
  fun simplifyTerm e =
      termMap evarId (fn (_, t) => fullySimplify t) 0 e


  (* Shifting and substitution. *)
  fun typeShiftAbove d c t =
      typeMap (fn (c, k, x) => L.TVar (if k < c then k else k + d, x)) tevarId c t
  fun typeShift d t = typeShiftAbove d 0 t
  fun typeSubst t k t' =
      typeMap (fn (c, k, x) => if k = c then typeShift c t else L.TVar (k, x)) tevarId k t'
  fun typeSubstTop e e' = typeShift (~1) (typeSubst (typeShift 1 e) 0 e')


  fun termShiftAbove d c e =
      termMap
          (fn (c, k, x) => L.EVar (if k < c then k else k + d, x)) 
          (fn (c, t) => typeShiftAbove d c t)
          c e
  fun termShift d e = termShiftAbove d 0 e

  fun termSubst e k e' =
      termMap 
          (fn (c, k, x) => if k = c then termShift c e else L.EVar (k, x))
          typId
          k e'
  fun termSubstTop e e' = termShift (~1) (termSubst (termShift 1 e) 0 e')

  fun typeTermSubst t a e =
      termMap
          evarId
          (fn (a, t') => typeSubst t a t')
          a e
  fun typeTermSubstTop t e = termShift (~1) (typeTermSubst (typeShift 1 t) 0 e)

  fun typeEvSubst t ev t' =
      typeMap tvarId (fn (c, ev') => if ev = ev' then typeShift c t else L.TEVar ev') 0 t'
  fun typeTermEvSubst t ev e =
      termMap
          (fn (_, k, x) => L.EVar (k, x))
          (fn (c, t') => typeEvSubst (typeShift c t) ev t')
          0 e



  (* equality - checks for alpha equivalence *)

  fun typEq t1 t2 = 
      case (t1, t2) of
          (L.TVar (k1, _), L.TVar (k2, _)) => k1 = k2
        | (L.TForall t1', L.TForall t2') => typEq t1' t2'
        | (L.TArrow (t1', t1''), L.TArrow (t2', t2'')) => 
          typEq t1' t2' andalso typEq t1'' t2''
        | (L.TUnit, L.TUnit) => true
        | (L.TProd (t1', t1''), L.TProd (t2', t2'')) => 
          typEq t1' t2' andalso typEq t1'' t2''
        | (L.TSum (t1', t1''), L.TSum (t2', t2'')) => 
          typEq t1' t2' andalso typEq t1'' t2''
        | (L.TList t1', L.TList t2') => typEq t1' t2'
        | (L.TEVar r1, L.TEVar r2) => r1 = r2
        | _ => false

  fun termEq e1 e2 = 
      case (e1, e2) of
          (L.EVar (k1, _), L.EVar (k2, _)) => k1 = k2
        | (L.EApp (e1', e1''), L.EApp (e2', e2'')) => 
          termEq e1' e2' andalso termEq e1'' e2''
        | (L.ELam (t1, e1'), L.ELam (t2, e2')) =>
          typEq t1 t2 andalso termEq e1' e2'
        | (L.EPlam e1', L.EPlam e2') =>
          termEq e1' e2'
        | (L.EFix (t1, e1'), L.EFix (t2, e2')) =>
          typEq t1 t2 andalso termEq e1' e2'
        | (L.ELet (e1', e1''), L.ELet (e2', e2'')) => 
          termEq e1' e2' andalso termEq e1'' e2''
        | (L.EUnit, L.EUnit) => true
        | (L.EPair (e1', e1''), L.EPair (e2', e2'')) => 
          termEq e1' e2' andalso termEq e1'' e2''
        | (L.EFst e1', L.EFst e2') => termEq e1' e2'
        | (L.ESnd e1', L.ESnd e2') => termEq e1' e2'
        | (L.EInl (e1', t1), L.EInl (e2', t2)) =>
          typEq t1 t2 andalso termEq e1' e2'
        | (L.EInr (e1', t1), L.EInr (e2', t2)) =>
          typEq t1 t2 andalso termEq e1' e2'
        | (L.ECase (e1', e1'', e1'''), L.ECase (e2', e2'', e2''')) =>
          termEq e1' e2' andalso termEq e1'' e2'' andalso termEq e1''' e2'''
        | (L.ERollList e1', L.ERollList e2') => termEq e1' e2'
        | (L.EUnrollList e1', L.EUnrollList e2') => termEq e1' e2'
        | _ => false

  (* Formats a type variable in the style that SML/NJ does.
   * This is bug compatible with SML/NJ in that it never goes
   * past 2 character names and starts using characters past
   * the end of lower case ASCII. *)
  fun fmtTypeVar k =
      let fun letter n = str (Char.chr (Char.ord #"a" + n))
          val letters = 26
      in "'" ^ (if k < letters then letter k
               else letter (k div letters) ^ letter (k mod letters))
      end

  (* Pretty printing *)
  (* For types, we make an effort to reduce the number of parens we print.
   * The precedence order is: * + -> list.
   * All of the binary constructors are right associative,
   * but we don't actually care about sum and product associativity. *)
  fun typeToStringHelp nice typ = 
      let fun prec (L.TArrow _) = 1
            | prec (L.TSum _) = 2
            | prec (L.TProd _) = 3
            | prec (L.TList _) = 4
            | prec _ = 5

          val typStr = typeToStringHelp nice
          fun parenType t = "(" ^ typStr t ^ ")"
          fun binTypeString t (t1, t2) s =
              (* All the binary operators are right associative, so
               * if the precedence is less than or equal on the left
               * we parenthesize and if the precedence is less on the
               * right we parenthesize. *)
              (if prec t1 <= prec t then parenType else typStr) t1
              ^ s ^
              (if prec t2 < prec t then parenType else typStr) t2

          val typ' = simplify typ
      in
          case typ' of
              L.TVar (i, s) =>
              if nice then fmtTypeVar i
              else s ^ "[" ^ Int.toString i ^ "]"
            | L.TEVar (ref (L.Free i)) => "?.X" ^ Int.toString i
            | L.TEVar (ref (L.Unified _)) => raise Fail "term not simplified!"
            | L.TForall t =>
              if nice then typStr t else
              "all(" ^ typStr t ^ ")"
            | L.TArrow ts => binTypeString typ' ts " -> "
            | L.TProd ts => binTypeString typ' ts " * "
            | L.TSum ts => binTypeString typ' ts " + "
            | L.TList L.TUnit => "nat"
            | L.TList t => (if prec t < prec typ'
                            then parenType else typStr) t ^ " list"
            | L.TUnit => "unit"
      end
  
  val typeToString = typeToStringHelp false
  val typeToStringNice = typeToStringHelp true

  fun bracketType t = "[" ^ typeToString t ^ "]"

  fun termToString exp = 
      case exp of 
          L.EVar (k, s) => s ^ "[" ^ Int.toString k ^ "]"
        | L.EApp (e1, e2) => parensTerm e1 ^ " " ^ parensTerm e2
        | L.ELam (t, e) => "lam" ^ bracketType t ^ parensTerm e
        | L.EPlam e => "plam" ^ parensTerm e
        | L.EPapp (e, t) => parensTerm e ^ "[" ^ typeToString t ^ "]"
        | L.EFix (t, e) => "fix" ^ bracketType t ^ parensTerm e
        | L.ELet (e1, e2) => "let(" ^ termToString e1 ^ ", " ^ termToString e2 ^ ")"
        | L.EUnit => "()"
        | L.EPair (e1, e2) => "<" ^ termToString e1 ^ ", " ^ termToString e2 ^ ">"
        | L.EFst e => "fst" ^ parensTerm e
        | L.ESnd e => "snd" ^ parensTerm e
        | L.EInl (e, t) => "inl" ^ bracketType t ^ parensTerm e
        | L.EInr (e, t) => "inr" ^ bracketType t ^ parensTerm e
        | L.ECase (e, eLeft, eRight) =>
          "case(" ^ termToString e ^ ", " ^ termToString eLeft ^ ", " ^ termToString eRight ^ ")"
        | L.ERollList e => "roll(" ^ termToString e ^ ")"
        | L.EUnrollList e => "unroll(" ^ termToString e ^ ")"
  and parensTerm exp = "(" ^ termToString exp ^ ")"


end
