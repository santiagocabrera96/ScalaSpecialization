package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = 
      namedExpressions mapValues { signalExpr => Signal {
          if(checkCicles(signalExpr(), namedExpressions)(Set())) 
            eval(signalExpr(), namedExpressions)
          else Double.NaN
        }
      }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(name) => if(references contains name) eval(references(name)(), references) else Double.NaN
    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
  }
  
  def checkCicles(expr: Expr, references: Map[String, Signal[Expr]])(implicit visited: Set[String]) : Boolean = expr match {
    case Literal(_) => true
    case Ref(name) => if(visited contains name) false else checkCicles(references(name)(), references)(visited + name)
    case Plus(a, b) => checkCicles(a, references) && checkCicles(b, references)
    case Minus(a, b) => checkCicles(a, references) && checkCicles(b, references)
    case Times(a, b) => checkCicles(a, references) && checkCicles(b, references)
    case Divide(a, b) => checkCicles(a, references) && checkCicles(b, references)
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
