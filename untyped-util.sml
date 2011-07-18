structure UntypedUtil :> UNTYPED_UTIL =
struct
  structure L = MinMLUntyped

  fun termToString exp = 
      case exp of 
          L.EVar (k, s) => s ^ "[" ^ Int.toString k ^ "]"
        | L.EApp (e1, e2) => parensTerm e1 ^ parensTerm e2
        | L.ELam e => "lam" ^ parensTerm e
        | L.EFix e => "fix" ^ parensTerm e
        | L.ELet (e1, e2) => "let(" ^ termToString e1 ^ ", " ^ termToString e2 ^ ")"
        | L.EUnit => "()"
        | L.EPair (e1, e2) => "<" ^ termToString e1 ^ ", " ^ termToString e2 ^ ">"
        | L.EFst e => "fst" ^ parensTerm e
        | L.ESnd e => "snd" ^ parensTerm e
        | L.EInl e => "inl" ^ parensTerm e
        | L.EInr e => "inr" ^ parensTerm e
        | L.ECase (e, eLeft, eRight) =>
          "case(" ^ termToString e ^ ", " ^ termToString eLeft ^ ", " ^ termToString eRight ^ ")"
        | L.ERollList e => "roll(" ^ termToString e ^ ")"
        | L.EUnrollList e => "unroll(" ^ termToString e ^ ")"
  and parensTerm exp = "(" ^ termToString exp ^ ")"


end
