signature TOPLEVEL =
sig
  val eval : string -> string * MinMLTyped.expr * MinMLTyped.typ
  val repl : unit -> unit
  val replFile : string -> unit
end

