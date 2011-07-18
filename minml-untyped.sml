structure MinMLUntyped =
struct
  (* datatype typ = datatype MinML.typ *)

  datatype expr =
           EVar of (int * string) (* carries a name for printing *)
         | EApp of expr * expr
         | ELam of expr (* binds a variable *)
         | EFix of expr (* binds a variable *)
         | ELet of expr * expr (* binds a variable in arg 2 *)
         | EUnit
         | EPair of expr * expr
         | EFst of expr
         | ESnd of expr
         | EInl of expr
         | EInr of expr
         | ECase of expr * expr * expr (* binds a variable in args 2 and 3 *)
         | ERollList of expr
         | EUnrollList of expr
end
