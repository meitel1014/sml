use "lib.sml";

fun compute s =
  let
    fun EXP nil = raise SyntaxError
      | EXP (h::t) =
      if isInt h then (toInt h, t)
      else if h="(" then COMP t
      else raise SyntaxError

    and COMP nil = raise SyntaxError
      | COMP (h::t) =
      let
        val (v1,t1) = EXP t
        val (v2,t2) = EXP t1
        val (v3,t3) = (hd t2,tl t2)
      in
        if v3 <> ")" then raise SyntaxError
        else
          if h = "+" then (v1 + v2, t3)
          else if h="-" then (v1 - v2, t3)
          else if h="*" then (v1 * v2, t3)
          else if h="/" then (v1 div v2, t3)
          else raise SyntaxError
      end
  in
    let
      val (result,rest) = EXP (separate s)
    in
      if rest = nil then result else raise SyntaxError
    end
  end;
