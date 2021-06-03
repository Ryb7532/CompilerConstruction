package nonscala
import Base._
import Abssyn._
import Oper._
import Ty._

object TypeCheck {

  case class TypeError(s: String) extends Throwable
  def typeError(s: String) = throw(TypeError(s))

  case class FuncTy(ts: List[Ty], result: Ty)

  // 式 e の型を返す
  // 型エラーがあった場合は, 例外 TypeError を発生させる
  // tcheck
  def tcheck (fenv: Map[Var, FuncTy], env: Map[Var, Ty], e: Exp) : Ty =
    e match {
      case VarExp(x) =>
        env.get(x) match {
          case Some(t) => t
          case None => typeError(s"Cannot find variable: $x")
        }
      case IntExp(_) => IntTy
      case NilExp => IntListTy
      case UOpExp(o, e1) => {
        val t1 = tcheck(fenv, env, e1)
        (o, t1) match {
          case (IsEmptyOp, IntListTy) => BoolTy
          case (HeadOp, IntListTy) => {
            if (e1 != NilExp) IntTy
            else typeError("head of empty list")
          }
          case (TailOp, IntListTy) => {
            if (e1 != NilExp) IntListTy
            else typeError("tail of empty list")
          }
          case _ => typeError("UOpExp")
        }
      }
      case BOpExp(o, e1, e2) => {
        val t1 = tcheck(fenv, env, e1)
        val t2 = tcheck(fenv, env, e2)
        (o, t1, t2) match {
          case (PlusOp, IntTy, IntTy) => IntTy
          case (MinusOp, IntTy, IntTy) => IntTy
          case (TimesOp, IntTy, IntTy) => IntTy
          case (DivideOp, IntTy, IntTy) => {
            if (e2 != IntExp(0)) IntTy
            else typeError("/ by zero")
          }
          case (EqOp, _, _) => {
            if (t1 == t2) BoolTy
            else typeError("type mismatch")
          }
          case (LtOp,  IntTy, IntTy) => BoolTy
          case (ConsOp, IntTy, IntListTy) => IntListTy
          case (ConsOp, IntTy, IntTy) => IntListTy
          case _ => typeError("BOpExp")
        }
      }
      case IfExp(e, e1, e2) => {
        val t = tcheck(fenv, env, e)
        val t1 = tcheck(fenv, env, e1)
        val t2 = tcheck(fenv, env, e2)
        t match {
          case BoolTy =>
            if (t1 == t2) t1
            else typeError("type mismatch")
          case _ => typeError("type mismatch")
        }
      }
      case AppExp(f, es) => {
        val FuncTy(ts,t) = fenv(f)
        if (ts == es.map(x => tcheck(fenv, env, x))) t
        else typeError("type mismatch")
        // Listのmapを使うと簡単
      }
    }

  def defs2fenv (ds: List[Def]): Map[Var, FuncTy] =
    ds.map(d =>
      (d.name, FuncTy(d.args.map(_._2), d.rtype))).toMap

  // 型エラーがあった場合は, 例外 TypeError を発生させる
  def tcheckDefs (ds: List[Def]): Unit = {
    val fenv = defs2fenv(ds)
    for (d <- ds)
      if (tcheck(fenv, d.args.toMap, d.body) != d.rtype)
        typeError(s"Type error: ${d.name}")

  }
}
