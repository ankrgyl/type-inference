local
    structure L = MinMLUntyped
in

signature UNTYPED_UTIL =
sig
  (* returns a string representation of a term  *)
  val termToString : L.expr -> string
end

end
