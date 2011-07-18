functor ExpLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Exp_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure L = MinMLNamedUntyped
exception Parse of string

fun createlet (var, e1) e2 = L.ELet (var, e1, e2)
fun createfun vars funend = foldl L.ELam funend vars
fun createcase e (xl,el) (xr,er) = L.ECase (e, xl, el, xr, er)

fun createnat 0 = L.ERollList (L.EInl L.EUnit)
  | createnat n = L.ERollList (L.EInr (L.EPair (L.EUnit, createnat (n-1))))

fun letify e decls = foldl (fn ((x, e1), e) => L.ELet (x, e1, e)) e decls


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\010\000\000\000\000\000\
\\001\000\002\000\021\000\003\000\020\000\004\000\032\000\005\000\019\000\
\\014\000\016\000\016\000\015\000\017\000\014\000\019\000\031\000\
\\020\000\013\000\021\000\012\000\022\000\011\000\023\000\010\000\
\\024\000\009\000\000\000\
\\001\000\002\000\021\000\003\000\020\000\005\000\019\000\006\000\018\000\
\\013\000\017\000\014\000\016\000\016\000\015\000\017\000\014\000\
\\020\000\013\000\021\000\012\000\022\000\011\000\023\000\010\000\
\\024\000\009\000\000\000\
\\001\000\002\000\021\000\003\000\020\000\005\000\019\000\014\000\016\000\
\\016\000\015\000\017\000\014\000\020\000\013\000\021\000\012\000\
\\022\000\011\000\023\000\010\000\024\000\009\000\000\000\
\\001\000\002\000\021\000\003\000\020\000\005\000\019\000\024\000\009\000\000\000\
\\001\000\002\000\026\000\000\000\
\\001\000\002\000\027\000\000\000\
\\001\000\002\000\028\000\000\000\
\\001\000\002\000\034\000\011\000\042\000\000\000\
\\001\000\002\000\034\000\012\000\033\000\000\000\
\\001\000\002\000\051\000\000\000\
\\001\000\002\000\056\000\000\000\
\\001\000\004\000\039\000\015\000\038\000\000\000\
\\001\000\004\000\049\000\000\000\
\\001\000\004\000\059\000\000\000\
\\001\000\006\000\018\000\007\000\037\000\013\000\017\000\000\000\
\\001\000\006\000\018\000\013\000\017\000\000\000\
\\001\000\008\000\048\000\000\000\
\\001\000\009\000\046\000\000\000\
\\001\000\011\000\036\000\000\000\
\\001\000\012\000\052\000\000\000\
\\001\000\012\000\057\000\000\000\
\\001\000\016\000\050\000\000\000\
\\001\000\017\000\055\000\000\000\
\\001\000\018\000\054\000\000\000\
\\061\000\006\000\018\000\013\000\017\000\000\000\
\\062\000\000\000\
\\063\000\000\000\
\\064\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\002\000\021\000\003\000\020\000\005\000\019\000\024\000\009\000\000\000\
\\070\000\000\000\
\\071\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\"
val actionRowNumbers =
"\002\000\041\000\033\000\004\000\
\\026\000\025\000\027\000\046\000\
\\040\000\039\000\038\000\037\000\
\\036\000\035\000\005\000\006\000\
\\007\000\016\000\001\000\044\000\
\\043\000\042\000\028\000\009\000\
\\029\000\005\000\019\000\015\000\
\\012\000\003\000\049\000\003\000\
\\030\000\008\000\003\000\003\000\
\\003\000\050\000\018\000\034\000\
\\003\000\031\000\017\000\013\000\
\\022\000\032\000\045\000\048\000\
\\010\000\020\000\003\000\024\000\
\\023\000\011\000\021\000\003\000\
\\014\000\047\000\000\000"
val gotoT =
"\
\\001\000\058\000\002\000\006\000\003\000\005\000\005\000\004\000\
\\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\008\000\020\000\000\000\
\\008\000\021\000\000\000\
\\000\000\
\\002\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\023\000\000\000\
\\000\000\
\\000\000\
\\002\000\006\000\003\000\027\000\000\000\
\\005\000\028\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\033\000\000\000\
\\000\000\
\\002\000\022\000\000\000\
\\000\000\
\\005\000\038\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\005\000\039\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\005\000\041\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\005\000\042\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\005\000\043\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\045\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\051\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\056\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 59
val numrules = 26
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUMBER of unit ->  (int) | IDENT of unit ->  (string)
 | atomic_exp of unit ->  (L.expr)
 | atomic_exp_list of unit ->  (L.expr)
 | builtin of unit ->  (L.expr -> L.expr) | exp of unit ->  (L.expr)
 | funargs of unit ->  (string list)
 | decls of unit ->  ( ( string * L.expr )  list)
 | decl of unit ->  (string*L.expr)
 | start of unit ->  ( ( string * L.expr )  list)
end
type svalue = MlyValue.svalue
type result =  ( string * L.expr )  list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | (T 9) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "IDENT"
  | (T 2) => "LPAREN"
  | (T 3) => "RPAREN"
  | (T 4) => "LET"
  | (T 5) => "VAL"
  | (T 6) => "IN"
  | (T 7) => "END"
  | (T 8) => "OF"
  | (T 9) => "SEMI"
  | (T 10) => "EQUALS"
  | (T 11) => "GOESTO"
  | (T 12) => "FUN"
  | (T 13) => "FN"
  | (T 14) => "COMMA"
  | (T 15) => "INL"
  | (T 16) => "INR"
  | (T 17) => "BAR"
  | (T 18) => "CASE"
  | (T 19) => "FST"
  | (T 20) => "SND"
  | (T 21) => "ROLL"
  | (T 22) => "UNROLL"
  | (T 23) => "NUMBER"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.decls decls1, decls1left, decls1right)) :: 
rest671)) => let val  result = MlyValue.start (fn _ => let val  (decls
 as decls1) = decls1 ()
 in (decls)
end)
 in ( LrTable.NT 0, ( result, decls1left, decls1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.start (fn _ => let val  (exp as exp1) =
 exp1 ()
 in ([("it", exp)])
end)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decl decl1, decl1left, decl1right)) :: 
rest671)) => let val  result = MlyValue.decls (fn _ => let val  (decl
 as decl1) = decl1 ()
 in ([decl])
end)
 in ( LrTable.NT 2, ( result, decl1left, decl1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.decl decl1, _, decl1right)) :: ( _, ( 
MlyValue.decls decls1, decls1left, _)) :: rest671)) => let val  result
 = MlyValue.decls (fn _ => let val  (decls as decls1) = decls1 ()
 val  (decl as decl1) = decl1 ()
 in (decl::decls)
end)
 in ( LrTable.NT 2, ( result, decls1left, decl1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.funargs (fn _ => let val  (
IDENT as IDENT1) = IDENT1 ()
 in ([IDENT])
end)
 in ( LrTable.NT 3, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.IDENT IDENT1, _, IDENT1right)) :: ( _, ( 
MlyValue.funargs funargs1, funargs1left, _)) :: rest671)) => let val  
result = MlyValue.funargs (fn _ => let val  (funargs as funargs1) = 
funargs1 ()
 val  (IDENT as IDENT1) = IDENT1 ()
 in (IDENT::funargs)
end)
 in ( LrTable.NT 3, ( result, funargs1left, IDENT1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671))
 => let val  result = MlyValue.decl (fn _ => let val  (IDENT as IDENT1
) = IDENT1 ()
 val  (exp as exp1) = exp1 ()
 in (IDENT, exp)
end)
 in ( LrTable.NT 1, ( result, VAL1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.funargs funargs1, _, _)) :: ( _, ( MlyValue.IDENT IDENT1, _,
 _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = 
MlyValue.decl (fn _ => let val  (IDENT as IDENT1) = IDENT1 ()
 val  (funargs as funargs1) = funargs1 ()
 val  (exp as exp1) = exp1 ()
 in (IDENT, L.EFix (IDENT, createfun funargs exp))
end)
 in ( LrTable.NT 1, ( result, FUN1left, exp1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.atomic_exp_list atomic_exp_list1, 
atomic_exp_list1left, atomic_exp_list1right)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  (atomic_exp_list as 
atomic_exp_list1) = atomic_exp_list1 ()
 in (atomic_exp_list)
end)
 in ( LrTable.NT 4, ( result, atomic_exp_list1left, 
atomic_exp_list1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.funargs funargs1, _, _)) :: ( _, ( _, FN1left, _)) :: rest671
)) => let val  result = MlyValue.exp (fn _ => let val  (funargs as 
funargs1) = funargs1 ()
 val  (exp as exp1) = exp1 ()
 in (createfun funargs exp)
end)
 in ( LrTable.NT 4, ( result, FN1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( _, INL1left, INL1right)) :: rest671)) => let val  
result = MlyValue.builtin (fn _ => (L.EInl))
 in ( LrTable.NT 5, ( result, INL1left, INL1right), rest671)
end
|  ( 11, ( ( _, ( _, INR1left, INR1right)) :: rest671)) => let val  
result = MlyValue.builtin (fn _ => (L.EInr))
 in ( LrTable.NT 5, ( result, INR1left, INR1right), rest671)
end
|  ( 12, ( ( _, ( _, FST1left, FST1right)) :: rest671)) => let val  
result = MlyValue.builtin (fn _ => (L.EFst))
 in ( LrTable.NT 5, ( result, FST1left, FST1right), rest671)
end
|  ( 13, ( ( _, ( _, SND1left, SND1right)) :: rest671)) => let val  
result = MlyValue.builtin (fn _ => (L.ESnd))
 in ( LrTable.NT 5, ( result, SND1left, SND1right), rest671)
end
|  ( 14, ( ( _, ( _, ROLL1left, ROLL1right)) :: rest671)) => let val  
result = MlyValue.builtin (fn _ => (L.ERollList))
 in ( LrTable.NT 5, ( result, ROLL1left, ROLL1right), rest671)
end
|  ( 15, ( ( _, ( _, UNROLL1left, UNROLL1right)) :: rest671)) => let
 val  result = MlyValue.builtin (fn _ => (L.EUnrollList))
 in ( LrTable.NT 5, ( result, UNROLL1left, UNROLL1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.atomic_exp atomic_exp1, atomic_exp1left, 
atomic_exp1right)) :: rest671)) => let val  result = 
MlyValue.atomic_exp_list (fn _ => let val  (atomic_exp as atomic_exp1)
 = atomic_exp1 ()
 in (atomic_exp)
end)
 in ( LrTable.NT 6, ( result, atomic_exp1left, atomic_exp1right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.atomic_exp atomic_exp1, _, atomic_exp1right
)) :: ( _, ( MlyValue.builtin builtin1, builtin1left, _)) :: rest671))
 => let val  result = MlyValue.atomic_exp_list (fn _ => let val  (
builtin as builtin1) = builtin1 ()
 val  (atomic_exp as atomic_exp1) = atomic_exp1 ()
 in (builtin atomic_exp)
end)
 in ( LrTable.NT 6, ( result, builtin1left, atomic_exp1right), rest671
)
end
|  ( 18, ( ( _, ( MlyValue.atomic_exp atomic_exp1, _, atomic_exp1right
)) :: ( _, ( MlyValue.atomic_exp_list atomic_exp_list1, 
atomic_exp_list1left, _)) :: rest671)) => let val  result = 
MlyValue.atomic_exp_list (fn _ => let val  (atomic_exp_list as 
atomic_exp_list1) = atomic_exp_list1 ()
 val  (atomic_exp as atomic_exp1) = atomic_exp1 ()
 in (L.EApp (atomic_exp_list, atomic_exp))
end)
 in ( LrTable.NT 6, ( result, atomic_exp_list1left, atomic_exp1right),
 rest671)
end
|  ( 19, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.atomic_exp (fn _ => let val  (
IDENT as IDENT1) = IDENT1 ()
 in (L.EVar IDENT)
end)
 in ( LrTable.NT 7, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 20, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp exp1, _, _)
) :: _ :: ( _, ( MlyValue.decls decls1, _, _)) :: ( _, ( _, LET1left,
 _)) :: rest671)) => let val  result = MlyValue.atomic_exp (fn _ =>
 let val  (decls as decls1) = decls1 ()
 val  (exp as exp1) = exp1 ()
 in (letify exp decls)
end)
 in ( LrTable.NT 7, ( result, LET1left, END1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.NUMBER NUMBER1, NUMBER1left, NUMBER1right))
 :: rest671)) => let val  result = MlyValue.atomic_exp (fn _ => let
 val  (NUMBER as NUMBER1) = NUMBER1 ()
 in (createnat NUMBER)
end)
 in ( LrTable.NT 7, ( result, NUMBER1left, NUMBER1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp3, _,
 _)) :: _ :: ( _, ( MlyValue.IDENT IDENT2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.IDENT IDENT1, _, _))
 :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( _, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.atomic_exp
 (fn _ => let val  exp1 = exp1 ()
 val  IDENT1 = IDENT1 ()
 val  exp2 = exp2 ()
 val  IDENT2 = IDENT2 ()
 val  exp3 = exp3 ()
 in (createcase exp1 (IDENT1,exp2) (IDENT2,exp3))
end)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 23, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, LPAREN1left
, _)) :: rest671)) => let val  result = MlyValue.atomic_exp (fn _ =>
 let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (L.EPair (exp1, exp2))
end)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.atomic_exp (fn _ => (
L.EUnit))
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atomic_exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Exp_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun GOESTO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun INL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun INR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun CASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun FST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun SND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun ROLL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun UNROLL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMBER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.NUMBER (fn () => i),p1,p2))
end
end
