

// partly borrowed from https://github.com/hallettj/LambdaCalculus

sealed abstract class Expression {

  def substitute(orig: Var, sub: Expression): Expression
  def freeVars: Set[Var]
  def boundVars: Set[Var]
  def printable: String

  def alphaConversion(conflicting: Set[Var]): Expression = this match {
    case Var(_) => this
    case Abstraction(arg, body) =>
      if (conflicting contains arg) {
        val newArg = arg.prime(conflicting ++ this.boundVars)
        Abstraction(newArg, body.substitute(arg, newArg))
      } else
        Abstraction(arg, body alphaConversion conflicting)
    case Application(a, b) =>
      Application(a alphaConversion conflicting, b alphaConversion conflicting)
    case other => sys.error(s"unimplemented alpha $other")
  }

  def betaReduction: Expression = this match {
    case Application(Abstraction(arg, body), b) => body.substitute(arg, b)
    case Application(a, b) =>
      val left = Application(a.betaReduction, b)
      if (left != this)
        left
      else
        Application(a, b.betaReduction)
    case Abstraction(arg, body) => Abstraction(arg, body.betaReduction)
    case _ => this
  }

  def etaConversion: Expression = this match {
    case Abstraction(x, Application(f, y)) if x == y =>
      if (f.freeVars contains x) this else f
    case _ => this
  }

  def expandCombinators: Expression = this match {
    case c:Combinator => c.expand
    case Application(a, b) => Application(a.expandCombinators, b.expandCombinators)
    case Abstraction(a, b) => Abstraction(a, b.expandCombinators)
    case _ => this
  }

  def normalFormLoop(callback: Expression => Unit): Expression = {
    callback(this)
    val beta = this.betaReduction
    if (beta != this)
      beta.normalFormLoop(callback)
    else {
      val eta = this.etaConversion
      if (eta != this) callback(eta)
      eta
    }
  }

  def normalForm(callback: Expression => Unit): Expression = {
    this.expandCombinators.normalFormLoop(callback)
  }

  def normalForm: Expression = normalForm { e => () }


  // attempt to produce compact results using the strategy from
  // https://tromp.github.io/cl/LC.pdf
  // uses SKI basis
  def eliminateAbstractionInner: Expression = this match {

    case Abstraction(arg@Var(_), body) => body match {

      //1   λx.SKM ≡ SK  (for all M)
      case Application(Application(CombS(), CombK()), _) =>
        Application(CombS(), CombK())

      //2 definition of K
      case _ if !body.freeVars.contains(arg) =>
        Application(CombK(), body)

      //3 definition of I
      case v: Var if v == arg => CombI()

      //4 λx.Mx ≡ M   (x not free in M)
      case Application(expr, v@Var(_))
        if arg == v && !expr.freeVars.contains(arg) =>
        expr

      //5 λx.xMx ≡ λx.SSKxM
      case Application(Application(v1@Var(_), expr), v2@Var(_))
        if arg == v1 && arg == v2 =>
        Abstraction(arg,
          Seq(CombS(), CombS(), CombK(), arg, expr)
            .reduceLeft(Application))

      //6 λx.M(NL) ≡ λx.S(λx.M)NL  (M, N combinators)
      case Application(m: Combinator, Application(n: Combinator, l)) =>
        Abstraction(arg,
          Seq(CombS(), Abstraction(arg, m), n, l)
            .reduceLeft(Application))

      //7 λx.MNL ≡ λx.SM(λx.L)N  (M, L combinators)
      case Application(Application(m: Combinator, n), l: Combinator) =>
        Abstraction(arg,
          Seq(CombS(), m, Abstraction(arg, l), n)
            .reduceLeft(Application))

      //8 λx.ML(NL) ≡ λx.SMLN  (M, N combinators)
      case Application(Application(m: Combinator, l1),Application(n: Combinator, l2))
        if l1 == l2 =>
        Abstraction(arg,
          Seq(CombS(), m, l1, n)
            .reduceLeft(Application))

      //9 definition of S
      case Application(expr1, expr2) =>
        Application(
          Application(
            CombS(),
            Abstraction(arg, expr1)),
          Abstraction(arg, expr2))

      case abs@Abstraction(arg2, body2) => Abstraction(arg, abs.eliminateAbstractionInner)

      case _ => this
    }

    case Application(a1, a2) =>
      Application(a1.eliminateAbstractionInner, a2.eliminateAbstractionInner)

    case _ => this
  }

  def eliminateAbstraction(callback: Expression => Unit): Expression = {
    callback(this)
    val eliminated = this.eliminateAbstractionInner
    if (eliminated == this)
      eliminated
    else
      eliminated.eliminateAbstraction(callback)
  }

  def eliminateAbstraction: Expression = eliminateAbstraction { e => () }

  def interpret: Int = {
    def interpretInner(f: Var, z: Var, inner:Expression, tot: Int):Int = inner match {
      case Application(f2@Var(_), inner2:Expression) if f == f2 =>
        interpretInner(f, z, inner2, tot + 1)
      case z2@Var(_) if z == z2 =>
        tot
      case _ => -1
    }
    this match {
      case Abstraction(f@Var(_),Abstraction(z@Var(_), inner:Expression)) =>
        interpretInner(f, z, inner, 0)
      case _ => -1
    }
  }
}

abstract class Combinator extends Expression {

  def substitute(orig: Var, sub: Expression): Expression = this
  def freeVars: Set[Var] = Set()
  def boundVars: Set[Var] = Set()
  def expand: Expression
  def printable: String
}

case class CombI() extends Combinator {
  override def expand: Expression =
    Abstraction(Var("x"), Var("x"))
  override def printable: String = "I"
}

case class CombK() extends Combinator {
  override def expand: Expression =
    Abstraction(Var("x"), Abstraction(Var("y"), Var("x")))
  override def printable: String = "K"
}

case class CombS() extends Combinator{
  override def expand: Expression =
    Abstraction(Var("x"), Abstraction(Var("y"), Abstraction(Var("z"),
      Application(Application(Var("x"),Var("z")), Application(Var("y"),Var("z"))))))
  override def printable: String = "S"
}

////////

case class Var(name: String) extends Expression {
  def substitute(orig: Var, sub: Expression): Expression =
    if (orig == this) sub else this

  def freeVars: Set[Var] = Set(this)

  def boundVars: Set[Var] = Set()

  override def printable: String = name

  /**
    * Adds prime (') to the variable name as many times as is necessary to get a
    * variable name that does not conflict with anything in `conflicting`.
    */
  def prime(conflicting: Set[Var]): Var = {
    val newVar = Var(name + "'")
    if (conflicting contains newVar)
      newVar.prime(conflicting)
    else
      newVar
  }

}

case class Abstraction(argument: Var, body: Expression) extends Expression {
  def substitute(orig: Var, sub: Expression): Expression = {
    if (orig != argument) {
      if (sub.freeVars contains argument)
        (this alphaConversion sub.freeVars).substitute(orig, sub)
      else
        Abstraction(argument, body.substitute(orig, sub))
    } else
      this
  }

  def freeVars: Set[Var] = body.freeVars - argument

  def boundVars: Set[Var] = body.boundVars + argument

  override def printable: String = String.format("λ%s.%s", argument.printable, body.printable)

  /**
    * `equals` is overriden here to capture alpha-equivalence.
    */
  override def equals(other: Any): Boolean = other match {
    case Abstraction(a, b) =>
      (a == argument && b == body) || body == b.substitute(a, argument)
    case _ => false
  }
}

case class Application(function: Expression, argument: Expression) extends Expression {
  def substitute(orig: Var, sub: Expression): Expression =
    Application(function.substitute(orig, sub), argument.substitute(orig, sub))

  def freeVars: Set[Var] = function.freeVars ++ argument.freeVars

  def boundVars: Set[Var] = function.boundVars ++ argument.boundVars

  override def printable: String = {
    (function match {
      case Abstraction(_, _) => "(" + function.printable + ")"
      case _ => function.printable
    }) +
      (argument match {
        case Var(_) => " " + argument.printable
        case a if a.isInstanceOf[Combinator] => argument.printable
        case _ => "(" + argument.printable + ")"
      })
  }
}
