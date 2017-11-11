

import macros.Env

import scala.util.{Failure, Success, Try}

object whitespace extends fastparse.WhitespaceApi.Wrapper({
  import fastparse.all._
  NoTrace(" ".rep)
})

//noinspection ForwardReference
class parser(env: Env[Expression]) {

  import whitespace._
  import fastparse.noApi._

  val lambdaSymbol = P("\\" | "λ" | "L" | "^")

  val variable = P("$".? ~ (CharIn('a' to 'z') ~~ "'".rep).!.map { Var })

  val macroExpansion: P[Expression] = {
    val parsers = env.keys.toArray.sortBy(k => -k.length) map { k =>
      P(k)(sourcecode.Name("macro-" + k)).!.map { env.apply }
    }
    if (env.nonEmpty)
      parsers.reduceLeft { _ | _ }
    else
      Fail
  }

  val churchNumeral: P[Expression] = {
    P( "C" ~ CharIn( '0' to '9' ).rep(1).! )
      .map{n => arith.church(n.toInt)}
  }

  val simpleExpression =
    P(abstraction |
      variable |
      "(" ~ expression ~ ")" |
      "`" ~ expression |
      churchNumeral |
      macroExpansion
    )

  val abstraction: P[Expression] =
    P(lambdaSymbol ~ variable.rep(min = 1) ~ P("."|"->") ~ expression)
      .map { case (args, body) => (args :\ body) {Abstraction} }

  val application: P[Expression] =
    P(simpleExpression ~ simpleExpression.rep(min = 1))
      .map { case (e, e2) => (e /: e2) {Application} }

  val expression: P[Expression] = {
    P(application | simpleExpression)
  }

  val lambdaCalculusExpression: P[Expression] = {
    P(Start ~ expression ~ End)
  }
}

object helper {

  import fastparse.all._

  // todo: employ parser whitespace handler
  def stripComments(inp:String): String = {
    inp.replaceAll("#.*", " ")
       .replaceAll("\\s+", " ")
  }

  // parse the macro definitions
  // (number of loops depends on nesting depth)
  def parseEnv(env:Env[String],
               result:Env[Expression] = Map(),
               loopProtector:Int = 0): Env[Expression] = {
    if ((env.keySet subsetOf result.keySet) || loopProtector > env.size)
      // todo: better feedback on parse failures not related to macros
      result
    else {
      val parser = new parser(result)
      val newResult =
        (for {
          (k, v) <- env
          if !result.contains(k)
        } yield {
          parser.expression.parse(v) match {
            case Parsed.Success(expr, ind) if ind == v.length => Some(k, expr)
            case Parsed.Success(_, _) => None
            case Parsed.Failure(_, _, _) => None
          }
        }).flatten
      parseEnv(env, result ++ newResult, loopProtector + 1)
    }
  }

  def parse(inp: String): Parsed[Expression] = {
    val commentless = stripComments(inp)
    macros.macroParser(commentless) match {
      case p@Parsed.Success(value, _) =>
        val parsedEnv = parseEnv(value)
        val lambdaExpressionInp = commentless.substring(p.index)
        new parser(parsedEnv).lambdaCalculusExpression.parse(lambdaExpressionInp)
      case p@Parsed.Failure(_,_,_) =>
        p
    }
  }

  def parseTry(inp: String): Try[Expression] = {
    parse(inp) match {
      case Parsed.Success(exp, _) => Success(exp)
      case p@Parsed.Failure(_,_,_) => Failure(ParseError(p))
    }
  }

}

