
object macros {

  import whitespace._
  import fastparse.noApi._

  type Env[T] = Map[String, T]

  val combinators = Map(
    "S" -> "λxyz.xz(yz)",
    "K" -> "λxy.x",
    "I" -> "λx.x",
    "Y" -> "λf.(λz.f(zz))(λz.f(zz))",
    "B" -> "λxyz.x(yz)",
    "C" -> "λxyz.xzy",
    "W" -> "λxy.xyy",
    "J" -> "λabcd.ab(adc)",
    "IOTA" -> "λf.fSK"
  )

  val logicMacros = Map(
    "TRUE"   -> "λxy.x",
    "FALSE"  -> "λxy.y",
    "AND"    -> "λpq.pqp",
    "OR"     -> "λpq.ppq",
    "XOR"    -> "λpq.p(NOT q)q",
    "NOT"    -> "λpab.pba"
  )

  val arithMacros = Map(
    "SUCC"   -> "λnfx.f(nfx)",
    "PRED"   -> "λnfx.n(λgh.h(gf))(λu.x)(λu.u)",
    "PLUS"   -> "λmn.n SUCC m",
    "SUB"    -> "λmn.n PRED m",
    "MUL"    -> "λmnf.m(nf)",
    "ISZERO" -> "λn.n(λx.FALSE)TRUE",
    "POW"    -> "λab.ba",
    "EXPT"   -> "POW",
    "LEQ"    -> "λmn.ISZERO(SUB mn)",
    "LT"     -> "λab.NOT (LEQ ba)",
    "EQ"     -> "λmn.AND (LEQ mn) (LEQ nm)",
    "DIV"    -> "Y(λgqab.LT ab(PAIR qa)(g (SUCC q)(SUB ab)b)) C0",
    "IDIV"   -> "λab.CAR (DIV ab)",
    "MOD"    -> "λab.CDR (DIV ab)"
  )

  val listMacros = Map(
    "PAIR" -> "λxyp.pxy",
    "NIL"  -> "λx.TRUE",
    "CAR"  -> "λp.p TRUE",
    "CDR"  -> "λp.p FALSE"
  )

  // http://jwodder.freeshell.org/lambda.html
  val mathMacros = Map(
    "FIBONACCI"  -> "λn.n(λfab.f b         (PLUS ab)) K C0 C1",
    "FACTORIAL"  -> "λn.n(λfax.f (MUL a x) (SUCC x))  K C1 C1"
    //"FACTORIAL'" -> "Y(λfx.ISZERO x C1 (MUL x (f (PRED x))))"
  )

  val allMacros: Env[String] = combinators ++ logicMacros ++ arithMacros ++ listMacros ++ mathMacros

  val ws = " ".rep
  val lhs = P( CharIn('A' to 'Z', '0' to '9', Seq('\'') ).rep(1) )
  val rhs = P( CharPred(_ != ';').rep(1) )

  private def definitions(env: Env[String]): Parser[Env[String]] = {

    val assignment:Parser[(String,String)] =
      P(ws ~ lhs.! ~  "=" ~ rhs.! ~ ";"  ~ ws)

    val leadingAssignments = P( assignment ~ &( assignment ))
      .flatMap(entry => definitions(env + entry))

    val lastAssignment = P( assignment ~ !assignment)
      .map(env + _)

    P( leadingAssignments | lastAssignment | PassWith(env) )
  }

  def macroParser(inp: String): Parsed[Env[String]] = {
    val env = allMacros
    P(Start ~ definitions(env)).parse(inp)
  }

}