package nonscala

import Base._
import CodegenBase._
import Oper._

object IL {

  trait Val
  case class VarVal(s: Var) extends Val
  case class IntVal(i: Int) extends Val
  case object NilVal extends Val

  trait Exp
  case class ValExp(v: Val) extends Exp
  case class BOpExp(o: BOp, v1: Val, v2: Val) extends Exp
  case class UOpExp(o: UOp, v: Val) extends Exp
  case class CallExp(f: Var, vs: List[Val]) extends Exp

  trait CExp // 比較式
  case class IsEmptyCExp(v: Val) extends CExp
  case class EqCExp(v1: Val, v2: Val) extends CExp
  case class LtCExp(v1: Val, v2: Val) extends CExp

  sealed trait Instr
  case class AssignInstr(x: Var, e: Exp) extends Instr
  case class LabelInstr(l: Label) extends Instr
  case class GotoInstr(l: Label) extends Instr
  case class IfGotoInstr(c: CExp, l: Label) extends Instr

  type Code = List[Instr]

  case class Def(name: String, args: List[Var], code: Code, result: Val)

  def trans(e: Abssyn.Exp) : (Code, Val) =
    e match {
      case Abssyn.VarExp(x) => (Nil, VarVal(x))
      case Abssyn.IntExp(i) => (Nil, IntVal(i))
      case Abssyn.NilExp =>    (Nil, NilVal)
      case Abssyn.BOpExp(o, e1, e2) => {
        val x = freshVar()
        val (c1, v1) = trans(e1)
        val (c2, v2) = trans(e2)
        (c1 ++ c2 ++ List(AssignInstr(x, BOpExp(o, v1, v2))),
          VarVal(x))
      }
      case Abssyn.UOpExp(o, e) => {
        val x = freshVar()
        val (c, v) = trans(e)
        (c ++ List(AssignInstr(x, UOpExp(o, v))), VarVal(x))
      }
      case Abssyn.IfExp(e, e1, e2) => {
        val r = freshVar()
        val thenLabel = freshLabel("then") // thenから始まる新しいラベルを生成
        val nextLabel = freshLabel("next")
        val (c, ce) = transCmp(e)
        val (c1, v1) = trans(e1)
        val (c2, v2) = trans(e2)
        (c ++ List(IfGotoInstr(ce, thenLabel)) ++ c2
          ++ List(AssignInstr(r, ValExp(v2)), GotoInstr(nextLabel), LabelInstr(thenLabel))
          ++ c1 ++ List(AssignInstr(r, ValExp(v1)), LabelInstr(nextLabel)),
        VarVal(r))
      }
      case Abssyn.AppExp(f, es) => {
        val r = freshVar()
        val (l1, l2) = es.map(trans(_)).unzip
        (l1.flatten ++ List(AssignInstr(r, CallExp(f, l2))), VarVal(r))
                 // リストのメソッド: map, unzip, flatten を使うと簡単
      }
    }

  // if 文の条件部分eを翻訳
  def transCmp(e: Abssyn.Exp) : (Code, CExp) =
    e match {
      case Abssyn.BOpExp(EqOp, e1, e2) => {
        val (c1, v1) = trans(e1)
        val (c2, v2) = trans(e2)
        (c1 ++ c2, EqCExp(v1,v2))
      }
      case Abssyn.BOpExp(LtOp, e1, e2) => {
        val (c1, v1) = trans(e1)
        val (c2, v2) = trans(e2)
        (c1 ++ c2, LtCExp(v1,v2))
      }
      case Abssyn.UOpExp(IsEmptyOp, e1) => {
        val (c1, v1) = trans(e1)
        (c1, IsEmptyCExp(v1))
      }
      case _ => error()
    }

  def transDef(d: Abssyn.Def) : Def = {
    val (c, v) =  trans(d.body)
    Def(d.name, d.args.map(_._1), c, v)
  }

}
