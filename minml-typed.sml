structure MinMLTyped =
struct
  datatype typ =
           TVar of (int * string)
         | TForall of typ (* binds a variable *)
         | TArrow of typ * typ
         | TUnit
         | TProd of typ * typ
         | TSum of typ * typ
         | TList of typ
         | TEVar of evar
       and bind =
           Free of int
         | Unified of typ
  withtype evar = bind ref

  datatype expr =
           EVar of (int * string) (* carries a name for printing *)
         | EApp of expr * expr
         | ELam of typ * expr (* binds a variable *)
         | EPlam of expr (* binds a variable *)
         | EPapp of expr * typ

         | EFix of typ * expr (* binds a variable *)
         | ELet of expr * expr (* binds a variable in arg 2 *)
         | EUnit
         | EPair of expr * expr
         | EFst of expr
         | ESnd of expr
         | EInl of expr * typ
         | EInr of expr * typ
         | ECase of expr * expr * expr (* binds a variable in args 2 and 3 *)
         | ERollList of expr
         | EUnrollList of expr
end
