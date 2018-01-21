use "lib.sml";

fun compute s =
  let
    fun EXP nil = raise SyntaxError
      | EXP (h::t) =
      if isInt h then (toInt h, t)
      else if h="(" then
        if isAlp(hd t) then FUNC t
        else COMP t
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

    and FUNC nil = raise SyntaxError
      | FUNC (h::t) =
      let
        val (v1,t1) = EXP t
        val (v2,t2) = (hd t1,tl t1)
      in
        if v2 <> ")" then raise SyntaxError
        else
          if h="fact" then (fact v1, t2)
          else if h="fibo" then (fibo v1, t2)
          else raise SyntaxError
      end
  in
    let
      val (result,rest) = EXP (separate s)
    in
      if rest = nil then result else raise SyntaxError
    end
  end;
