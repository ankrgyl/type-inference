structure Test :> TEST =
struct

  structure LTYPED = MinMLTyped
  structure LTYPEDUTIL = TypedUtil
  structure INFER = Inference
  structure EVAL = Evaluator
  structure TOP = TopLevel

  (* This all ought to be in the basis library. Sigh. *)
  fun upto i j = if i > j then [] else i :: upto (i+1) j
  fun mapi f l = map f (ListPair.zip (upto 0 (length l-1), l))
  fun appi f l = app f (ListPair.zip (upto 0 (length l-1), l))

  datatype typeResult = 
           TypeFail of exn 
         | TypeSuccess of LTYPED.typ
  datatype evalResult = 
           EvalFail of exn 
         | InfiniteLoop 
         | EvalSuccess of string 
           (* use if you don't care about the result of evaluation, say 
             because you are only interested in type inference results or 
             something *)
         | EvalIgnore
  datatype fullResult = 
           (* use if you don't know or don't feel like specifying the specific
              cause of failure *)
           UnspecifiedFail
         | Specified of typeResult * evalResult
  (* (s, res) where 
        "s" is the MinML source program
        "res" is the expected result *)
  type fullTest = string * fullResult

  (***************************************************************************)
  (***************************************************************************)
  (***************************************************************************)
  (* Feel free to add your own test cases, but be aware that the
   * test-numbering will change.
   *
   * See the lab handout and the above comments for information on
   * how to write tests *)

  (* these are some DUMMY failure modes provided in order the specify
     correct test behavior *)
  val resTypeError = TypeFail (INFER.TypeError)
  val resOccursError = TypeFail (INFER.Occurs (ref (LTYPED.Free 0), LTYPED.TUnit))
  val resUnifyError = TypeFail (INFER.Unify (LTYPED.TUnit, LTYPED.TUnit))
  val resEvalError = InfiniteLoop
  val resStuckError = EvalFail (EVAL.Stuck LTYPED.EUnit)

  val p = LTYPED.TProd
  val u = LTYPED.TUnit

  val fullTests : fullTest list =
    [
      ("()", Specified (TypeSuccess LTYPED.TUnit, EvalSuccess "()")),
      ("x", UnspecifiedFail),
      ("(fn x => x)()", Specified (TypeSuccess LTYPED.TUnit, EvalSuccess "()")),
      ("let val f = fn x y => (x, y) in f () () end", Specified (TypeSuccess (LTYPED.TProd (LTYPED.TUnit, LTYPED.TUnit)), EvalSuccess "((), ())")),
      ("fun f x = x", Specified (TypeSuccess (LTYPED.TForall (LTYPED.TArrow (LTYPED.TVar (0, "t"), LTYPED.TVar (0, "t")))), EvalSuccess "fun f x = x")),
      ("fun f x = (case x of inl y => inr y | inr z => inl z);", Specified (TypeSuccess (LTYPED.TForall (LTYPED.TForall (LTYPED.TArrow (LTYPED.TSum (LTYPED.TVar (1, "t1"), LTYPED.TVar (0, "t2")), LTYPED.TSum (LTYPED.TVar (0, "t2"), LTYPED.TVar (1, "t1")))))), EvalIgnore)),
      ("fun f x = (case unroll x of inl _ => inl () | inr _ => inr ())", Specified (TypeSuccess (LTYPED.TForall (LTYPED.TArrow (LTYPED.TList (LTYPED.TVar (0, "t")), LTYPED.TSum (LTYPED.TUnit, LTYPED.TUnit)))), EvalIgnore)),
      ("let val f = () in f () end", Specified (resUnifyError, EvalIgnore)),
      ("fun f x = f f", Specified (resOccursError, EvalIgnore)),
      ("let val f = fn x => x in (f (), f ((), ())) end", Specified (TypeSuccess 
            (LTYPED.TProd (
              LTYPED.TUnit,
              LTYPED.TProd (
                LTYPED.TUnit,
                LTYPED.TUnit))), EvalSuccess "((), (() ()))")),
      ("fun f1 x = let val f2 = fn y => (x, y) in f2 () end; f1 ((), ());",
            Specified (TypeSuccess (p (p (u, u), u)), EvalIgnore)),
      ("roll (inl ())", Specified (TypeSuccess (LTYPED.TForall (LTYPED.TList
            (LTYPED.TVar (0, "t")))), EvalIgnore)),
      ("fun f x y = let fun p y = x in p y end",
            Specified (TypeSuccess (
                LTYPED.TForall (LTYPED.TForall (
                    LTYPED.TArrow (LTYPED.TVar (1, "t0"), LTYPED.TArrow (LTYPED.TVar (0, "t1"), LTYPED.TVar (1, "t0")))
                )
            )), EvalIgnore))
    ]

  (***************************************************************************)
  (***************************************************************************)
  (***************************************************************************)
  (********* You don't need to read below here unless you are interested *****)
  (***************************************************************************)
  (***************************************************************************)
  (***************************************************************************)








  (* generic representation of a test
   * name - the test name: TYPE.number
   * go - returns NONE if the test passed, and SOME msg if it failed
   * query - string representation of the query tested
   * expected - string representation of expected answer
   *)
  type test = {name : string, go : unit -> string option,
               query : string, expected_fn : unit -> string}
  type result = string option

  (* yay - code isn't terrible anymore.  why didn't i think of this earlier? *)

  (* verbose excepted reporting *)
  fun unexpectedExnToString ex = 
      case ex of 
          INFER.TypeError => "type error"
        | INFER.Occurs (_, t) => 
          "occurs check failed on " ^ LTYPEDUTIL.typeToString t
        | INFER.Unify (t1, t2) =>
          "unification failure on types " ^ LTYPEDUTIL.typeToString t1 ^
          " and " ^ LTYPEDUTIL.typeToString t2
        | EVAL.Stuck e => "stuck while evaluating " ^ LTYPEDUTIL.termToString e
        | x => exnMessage x

  (* used for pretty printing tests *)
  fun expectedExnToString ex = 
      case ex of 
          INFER.TypeError => "type error"
        | INFER.Occurs (_, t) => "occurs"
        | INFER.Unify (t1, t2) => "unify"
        | EVAL.Stuck e => "stuck"
        | x => exnMessage x

  fun externalResultToString res = 
      case res of
          UnspecifiedFail => "failure of some sort"
        | Specified (TypeFail ex, _) => expectedExnToString ex
        | Specified (TypeSuccess _, EvalFail ex) => expectedExnToString ex
        | Specified (TypeSuccess _, InfiniteLoop) => "infinite loop"
        | Specified (TypeSuccess t, EvalSuccess str) => 
          str ^ " : " ^ LTYPEDUTIL.typeToString t
        | Specified (TypeSuccess t, EvalIgnore) => 
          "inferred type of " ^ LTYPEDUTIL.typeToString t
  
  datatype typeResultInternal = 
           TypeFail' of exn
         | TypeSuccess' of LTYPED.typ
         | TypeIgnore'
         | UnspecifiedTypeFail'
  datatype evalResultInternal = 
           (* non termination is lumped in here *)
           EvalFail' of exn
         | EvalSuccess' of LTYPED.expr
         | EvalIgnore'
         | UnspecifiedEvalFail'
  datatype fullResultInternal = 
           Specified' of typeResultInternal * evalResultInternal
 
  (* so if I had more time, I'd implement this using a monad *)
  (* DONE means the test is done and has a result to report *)
  datatype testState = DONE of string option | NOTDONE
 
  val timeoutLenLong = Time.fromSeconds 10
  val timeoutLenShort = Time.fromSeconds 5
  val unnecessary = SOME "incorrectly raised an exception"

  fun typeResultToInternal (TypeFail x) = TypeFail' x
    | typeResultToInternal (TypeSuccess x) = TypeSuccess' x

  fun toInternalRep (src, Specified (typ_res, EvalSuccess str)) =  
      (case TOP.eval src of
          (_, exp, typ_res') => (src, Specified' (typeResultToInternal typ_res, EvalSuccess' exp)))
    | toInternalRep (src, Specified (typ_res, EvalFail ex)) =
      (src, Specified' (typeResultToInternal typ_res, EvalFail' ex))
    | toInternalRep (src, Specified (typ_res, InfiniteLoop)) = 
      (src, Specified' (typeResultToInternal typ_res, EvalFail' (Timeout.Timeout)))
    | toInternalRep (src, Specified (typ_res, EvalIgnore)) = 
      (src, Specified' (typeResultToInternal typ_res, EvalIgnore'))
    | toInternalRep (src, UnspecifiedFail) =
      (src, Specified' (UnspecifiedTypeFail', UnspecifiedEvalFail'))

  fun encodeResultExn ex = 
      case ex of
          INFER.TypeError => Specified' (TypeFail' ex, EvalIgnore')
        | INFER.Occurs _ => Specified' (TypeFail' ex, EvalIgnore')
        | INFER.Unify _ => Specified' (TypeFail' ex, EvalIgnore')
        | EVAL.Stuck _ => Specified' (TypeIgnore', EvalFail' ex)
        | Timeout.Timeout => Specified' (TypeIgnore', EvalFail' ex)
        | _ => Specified' (UnspecifiedTypeFail', UnspecifiedEvalFail')

  fun internalResultToString res = 
      case res of
          (TypeFail' ex, _) => expectedExnToString ex
        | (TypeSuccess' _, EvalFail' ex) => expectedExnToString ex
        | (TypeSuccess' t, EvalSuccess' e) => 
          LTYPEDUTIL.termToString e ^ " : " ^ LTYPEDUTIL.typeToString t
        | (TypeIgnore', EvalIgnore') => "ignored"
        | (TypeSuccess' t, EvalIgnore') => 
          "inferred type of " ^ LTYPEDUTIL.typeToString t
        | (UnspecifiedTypeFail', _) => "failure of some sort"
        | (_, UnspecifiedEvalFail') => "failure of some sort"
        | _ => "something went wrong when generating the test :("
  
  fun encodeInferResult actual expected = 
      case (actual, expected) of
          (TypeIgnore', _) => NOTDONE
        | (_, TypeIgnore') => NOTDONE
        | (UnspecifiedTypeFail', TypeFail' _) => DONE NONE
        | (UnspecifiedTypeFail', UnspecifiedTypeFail') => DONE NONE
        | (UnspecifiedTypeFail', _) => DONE (SOME "unexpected failure before/during type inference")
        | (TypeFail' _, UnspecifiedTypeFail') => DONE NONE
        | (TypeFail' _, TypeFail' _) => DONE NONE
        | (TypeSuccess' t_actual, TypeSuccess' t_expected) => 
          if LTYPEDUTIL.typEq t_actual t_expected then NOTDONE
          else DONE (SOME ("wrong inferred type. was " ^ 
                     LTYPEDUTIL.typeToStringNice t_actual))
        | (TypeFail' ex, _) => DONE (SOME (unexpectedExnToString ex))
        | (TypeSuccess' t_actual, _) => 
          DONE (SOME ("unexpected type inference succcess with" ^ 
               LTYPEDUTIL.typeToStringNice t_actual))

  fun encodeEvalResult' actual expected = 
      case (actual, expected) of 
          (EvalIgnore', _) => NOTDONE
        | (_, EvalIgnore') => NOTDONE
        | (UnspecifiedEvalFail', EvalFail' _) => DONE NONE
        | (UnspecifiedEvalFail', UnspecifiedEvalFail') => DONE NONE
        | (UnspecifiedEvalFail', _) => DONE (SOME "unexptected failure during evaluation")
        | (EvalFail' _, UnspecifiedEvalFail') => DONE NONE
        | (EvalFail' _, EvalFail' _) => DONE NONE
        | (EvalSuccess' e_actual, EvalSuccess' e_expected) => 
          if LTYPEDUTIL.termEq e_actual e_expected then DONE NONE
          else DONE (SOME ("wrong value. was " ^ LTYPEDUTIL.termToString e_actual))
        | (EvalFail' ex, _) => DONE (SOME (unexpectedExnToString ex))
        | (EvalSuccess' e_actual, _) => 
          DONE (SOME ("unexpected eval succcess with" ^ 
               LTYPEDUTIL.termToString e_actual))

  fun encodeFullResult (Specified' (t_actual, e_actual)) (Specified' (t_expected, e_expected)) = 
      case encodeInferResult t_actual t_expected of
          DONE (SOME str) => SOME str
        | DONE (NONE) => NONE
        | NOTDONE => 
          (case encodeEvalResult' e_actual e_expected of
              DONE (SOME str) => SOME str
            | DONE (NONE) => NONE
            | NOTDONE => 
              (case e_expected of 
                  EvalIgnore' => NONE 
                | _ => SOME "was expecting failure"))
         

  fun makeFullTest (k, theTest) =
      let 
          val g = fn () =>
          let val (src, expected) = toInternalRep theTest
              val Specified' (t_expected, e_expected) = expected
              val g_inner = 
                fn () => case TOP.eval src of 
                             (_, exp, typ) => encodeFullResult (Specified' (TypeSuccess' typ, EvalSuccess' exp)) expected
              val g_outer = 
                fn () => Timeout.runWithTimeoutExn timeoutLenLong g_inner () 
                           handle E => encodeFullResult (encodeResultExn E) expected
          in g_outer () end
          
          val (src, res) = theTest
          val q = src
          val ex_fn = fn () => externalResultToString res
      in {name = "FULL"^(Int.toString k), go = g, query = q, expected_fn = ex_fn} end

  val fullTests = mapi makeFullTest fullTests
  val allTests = fullTests


  (* functions to run tests, returning the results *)
  fun doTest ({go, ...} : test) =
      go ()
      handle E => SOME ("exception: " ^ exnMessage E)
  fun doTestN l n = doTest (List.nth (l, n)) 
      handle Subscript => SOME "no such test\n"
  val doTests = map doTest

  (* prints a message about a given test *)
  fun printTest ({name, ...}: test, result) =
        let val s = name ^ "\t\t" ^
                    (case result of
                         NONE => "pass"
                       | SOME msg => "fail with "  ^ msg)
                    ^ "\n"
        in print s end

  (* functions to run tests, printing the results *)
  fun runTest test = printTest (test, doTest test)
  fun runTestN l n = runTest (List.nth (l, n))
      handle Subscript => print "no such test\n"
  fun runTests tests =
      let val results = doTests tests
          val () = ListPair.app printTest (tests, results)
          val failed = length (List.filter isSome results)
          val () = print
                   ("********* " ^
                    (if failed = 0 then "All tests passed"
                     else "Failed "^(Int.toString failed)^"/"^
                          (Int.toString (length tests))^" tests") ^
                    " *********\n")
      in () end

  fun runAll () = runTests allTests

  fun printInfo {name, go, query, expected_fn} =
      print (name ^ " ==> " ^ query ^ " has expected result " ^
             (expected_fn ()) ^ "\n")
  fun printInfoN l n = printInfo (List.nth (l, n))
      handle Subscript => print "no such test\n"
end
