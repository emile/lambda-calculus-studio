
object arith {
  def church(n: Int): Expression = {
    val f = "f"
    val z = "z"
    Abstraction(
      Var(f),
      Abstraction(
        Var(z),
        (Var(z).asInstanceOf[Expression] /: (0 until n)) {
          (acc, _) => Application(Var(f), acc)
        }))
  }

  def interpretAsInt(expression: Expression): Option[Int] = {
    def count(f:Var, z:Var, expr:Expression, total: Int = 0): Option[Int] = expr match {
      case z2 if z == z2 => Some(total)
      case Application(f2:Var, apps) if f == f2 => count(f, z, apps, total + 1)
      case _ => None
    }
    expression match {
      case Abstraction(f:Var, Abstraction(z: Var, apps)) => count(f, z, apps)
      case _ => None
    }
  }

  def interpretAsBool(expression: Expression): Option[Boolean] = expression match {
    case Abstraction(x1:Var,Abstraction(y:Var,x2: Var)) if x1 == x2 && x1 != y =>
      Some(true)
    case Abstraction(x:Var,Abstraction(y1:Var,y2: Var)) if x != y1 && y1 == y2 =>
      Some(false)
    case _ => None
  }
}