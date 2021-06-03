import Abssyn._
import Base._

object Eval {
  def eval (env: Map[Var, Int], e: Exp) : Int =
    e match {
      case VarExp(x) => env.get(x) match {
        case Some(n) => n
        case None => 0
      }
      case IntExp(i) => i
      case PlusExp(e1, e2) => eval (env, e1) + eval (env, e2)
      case MinusExp(e1, e2) => eval (env, e1) - eval (env, e2)
      case TimesExp(e1, e2) => eval (env, e1) * eval (env, e2)
      case DevideExp(e1, e2) => eval (env, e1) / eval (env, e2)
    }
}



