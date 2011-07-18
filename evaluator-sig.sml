signature EVALUATOR =
sig
  exception Stuck of MinMLTyped.expr

  (* eval e: evaluate e down to a value *)
  val eval : MinMLTyped.expr -> MinMLTyped.expr
end
