/*
import minitest._

object lambdatest extends SimpleTestSuite {

  def isAbstractionFree(expr: Expression): Boolean = expr match {
    case Application(a, b) => isAbstractionFree(a) && isAbstractionFree(b)
    case c:Combinator => true
    case v:Var => true
    case Abstraction(_,_) => false
  }

  test("roundtrip"){
    Seq(
      "λyx.xy",
      "λyxz.xzy",
      "λabcd.dd(ab)",
      "λabcd.dd(ab)ccadb",
      "λabcd.adcdabb")
      .foreach(c => {

        val expr = parser.parse(c)
        val abstractionFree = expr.eliminateAbstraction
        assert(isAbstractionFree(abstractionFree))
        assertEquals(expr.normalForm, abstractionFree.normalForm)

      })
  }

}
*/