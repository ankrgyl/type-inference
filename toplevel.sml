structure TopLevel : TOPLEVEL =
struct

  structure INFER = Inference
  structure E = Evaluator
  structure L = MinMLTyped
  structure LU = MinMLUntyped
  structure U = TypedUtil
  structure C = Context

  exception InternalError

  (* Some printing/formatting utility functions *)
  fun println s = print (s ^ "\n")
  fun fmt_list' s f l = (String.concatWith s (map f l))
  fun fmt_list f l = "[" ^ (fmt_list' ", " f l) ^ "]"

  (* Tracking the current environment *)
  datatype env = Env of {symbols: string C.context,
                         values: L.expr list,
                         types: L.typ C.context}
  val emptyEnv = Env {symbols = C.empty, values = [], types = C.empty}
  fun updateEnvVar (Env {symbols, values, types}) var typ value =
      Env {symbols = C.bind symbols var,
           values = value :: values,
           types = C.bind types typ }

  (* Everything else. *)

  fun letify values e = foldl L.ELet e values

  fun evalCore (env as Env {values, types, symbols}) (id, exp) =
      let val exp = DeBruijnify.removeNames symbols exp
          (* val () = println (UntypedUtil.termToString exp) *)
          (*
          val _ = print ("-->\noriginal term is " ^ (UntypedUtil.termToString exp) ^ "\n")
          *)

          val (elab_typ, elab_exp) = INFER.generalize types (INFER.infer types exp)
          val (elab_typ', elab_exp') = (U.fullySimplify elab_typ,
                                        U.simplifyTerm elab_exp)
          (*
          val _ = print ("final type is " ^ (U.typeToString  elab_typ') ^ "\n")
          val _ = print ("final term is " ^ (U.termToString elab_exp') ^ "\n")
          *)
          val finexp = letify values elab_exp'
          (* val () = println (TypedUtil.termToString elab_exp') *)
          val checked_typ = Typechecker.typecheck C.empty finexp
          val () = if U.typEq elab_typ' checked_typ then ()
                   else raise Typechecker.TypeError (
                              "inferred and checked types do not match: " ^
                              U.typeToString elab_typ' ^ " <> " ^
                              U.typeToString checked_typ)
          val res = E.eval finexp
          (* val () = println (TypedUtil.termToString res) *)
          val newenv = updateEnvVar env id elab_typ' res
      in
          ((id, res, elab_typ'), newenv)
      end

  (* Convert a MinML list expression into an ML list *)
  fun expToList (L.ERollList (L.EInl _)) = []
    | expToList (L.ERollList (L.EInr (L.EPair (x, xs), _))) = x :: expToList xs
    | expToList _ = raise Fail "trying to expToList a non-list"

  fun resValToString exp =
      case exp of
          L.ELam _ => "fn"
        | L.EPlam e => resValToString e
        | L.EFix (_, e) => resValToString e
        | L.EPapp (e, _) => resValToString e
        | L.EUnit => "()"
        | L.EPair (e1, e2) => "(" ^ resValToString e1 ^ ", " ^ resValToString e2 ^ ")" 
        | L.EInl (e, _) => "inl(" ^ resValToString e ^ ")"
        | L.EInr (e, _) => "inr(" ^ resValToString e ^ ")"
        | L.ERollList _ => formatList exp
        | _ => "?"
  and formatList (L.ERollList (
                  L.EInl (_, L.TSum (_, L.TProd (L.TUnit, _))))) = "0"
    | formatList exp =
      let val exps = expToList exp
      in case exps of
             (* Format a unit list as an integer. *)
             L.EUnit :: _ => Int.toString (length exps)
           | _ => fmt_list resValToString exps
      end

  fun resToString (id, res_exp, res_type) =
      let
          val val_string = resValToString res_exp
          val typ_string = U.typeToStringNice res_type
      in
          ("val " ^ id ^ " = " ^ val_string
           ^ " : " ^ typ_string ^ "\n" )
      end
    
  fun processDef decl (res,env) =
      let
          val (res, env') = evalCore env decl
      in
          (SOME res, env')
      end
    
  fun processDefRepl text a =
      let
          val (res, env') = processDef text a
      in
          case res of
              SOME r => (TextIO.print (resToString r); (res, env'))
            | NONE => raise InternalError
      end

  fun errorMessage (Parse.Parse s) = "Parse error: " ^ s
    | errorMessage (INFER.Unify (t1, t2)) =
      "Could not unify " ^ U.typeToString t1 ^ " and " ^ U.typeToString t2
    | errorMessage (INFER.Occurs (ref (L.Free i), t)) =
      "?.X" ^ Int.toString i ^ " occurs in " ^ U.typeToString t
    | errorMessage (E.Stuck e) = "Stuck evaluating " ^ U.termToString e
    | errorMessage (Typechecker.TypeError s) =
      "Unexpected type error in generated term: " ^ s
    | errorMessage e = "Exception: " ^ exnMessage e

  fun printerror e = println (errorMessage e)

  fun hdl f x y = (f x y)
      handle e => (printerror e; y)

  (* The streams from the input library are of type (char * char list)
   * stream. The char list is "for error reporting". We don't care, so
   * we throw it out. *)
  fun cleanupStream s = Stream.map (fn (x, _) => x) s

  fun stringToStream s = 
      foldr Stream.cons Stream.empty (String.explode s)

  val evalStreamInteractive = Stream.fold (hdl processDefRepl) (NONE, emptyEnv)

  (* Produce a stream of parsed declarations from the repl. If the stream
   * dies due to a parse error, restart the stream. *)
  fun replStream () =
      Input.promptKeybd "-> " "=> "
      (Parse.parse (fn e => (printerror; replStream ())) o cleanupStream)

  (* Produce a stream of parsed declarations from a file. If the stream
   * dies due to a parse error, raise the exception. *)
  val fileStream = Parse.parse (fn e => (printerror e; raise InternalError)) o
                   cleanupStream o Input.readFile

  fun repl () = ignore (evalStreamInteractive (replStream ()))
  fun replFile s = ignore (evalStreamInteractive (
                           Stream.append (fileStream s, replStream ())))

  fun eval s =
      let 
          val stream = (Parse.parse (fn e => raise e) o stringToStream) s
          val (res, _) = Stream.fold processDef (NONE, emptyEnv) stream
      in
          case res of
              SOME r => r
            | NONE => (print "No result!\n"; raise InternalError)
      end
end
