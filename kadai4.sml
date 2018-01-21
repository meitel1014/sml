use "lib.sml";

fun compute s =
  let
    fun EXP nil = raise SyntaxError
      | EXP (h::t) =
      if isInt h then (toInt h, t)
      else if h="(" then OP t
      else raise SyntaxError

    and OP nil = raise SyntaxError
      | OP (h::t) =
      if h = "+" orelse h="-" orelse h="*" orelse h="/" then COMP (h::t)
      else raise SyntaxError

    and COMP nil = raise SyntaxError
      | COMP (h::t) =
      let
        val (v1,t1) = EXP t
        val (v2,t2) = EXP t1
      in
        if h = "+" then (v1 + v2, t2)
        else if h="-" then (v1 - v2, t2)
        else if h="*" then (v1 * v2, t2)
        else if h="/" then (v1 div v2, t2)
        else raise SyntaxError
      end
  in
    let
      val (result,rest) = EXP (separate s)
    in
      if (hd rest)=")" then result else raise SyntaxError
    end
  end;
