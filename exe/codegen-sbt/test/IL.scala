package nonscala

import org.scalatest._
import Base._
import Abssyn._
import Oper._
import ILExec._

class ILTest extends FlatSpec {

  def evalCV(fenv: FEnv, env: Env, cv: (IL.Code, IL.Val)): Value =
    evalVal(exec(fenv, env, cv._1), cv._2)

  "Var" should "正しい値" in
  {
    assert(evalCV(Map(), Map("x"->IntValue(1)), IL.trans(VarExp("x")))== IntValue(1))
    assert(evalCV(Map(), Map("x"->ListValue(List[Int]())), IL.trans(VarExp("x")))
      == ListValue(List[Int]()))
  }

  "Int" should "正しい値" in
  {
    assert(evalCV(Map(), Map(), IL.trans(IntExp(1))) == IntValue(1))
  }

  "Nil" should "正しい値" in
  {
    assert(evalCV(Map(), Map(), IL.trans(NilExp)) == ListValue(List[Int]()))
  }

  "+" should "正しい値" in
  {
    assert(evalCV(Map(), Map(),
      IL.trans(BOpExp(PlusOp, IntExp(1), IntExp(0)))) == IntValue(1))
    assert(evalCV(Map(), Map("x"->IntValue(1)),
      IL.trans(BOpExp(PlusOp, IntExp(1), VarExp("x"))))== IntValue(2))
    assert(evalCV(Map(), Map("x"->IntValue(1), "y"->IntValue(2)),
      IL.trans(BOpExp(PlusOp, VarExp("x"), VarExp("y")))) == IntValue(3))
  }

  "-(minus)" should "正しい値" in
  {
    assert(evalCV(Map(), Map(),
      IL.trans(BOpExp(MinusOp, IntExp(1), IntExp(0)))) == IntValue(1))
    assert(evalCV(Map(), Map("x"->IntValue(1)),
      IL.trans(BOpExp(MinusOp, IntExp(1), VarExp("x"))))== IntValue(0))
    assert(evalCV(Map(), Map("x"->IntValue(2), "y"->IntValue(1)),
      IL.trans(BOpExp(MinusOp, VarExp("x"), VarExp("y")))) == IntValue(1))
  }

  "*" should "正しい値" in
  {
        assert(evalCV(Map(), Map(),
      IL.trans(BOpExp(TimesOp, IntExp(1), IntExp(0)))) == IntValue(0))
    assert(evalCV(Map(), Map("x"->IntValue(1)),
      IL.trans(BOpExp(TimesOp, IntExp(1), VarExp("x"))))== IntValue(1))
    assert(evalCV(Map(), Map("x"->IntValue(2), "y"->IntValue(3)),
      IL.trans(BOpExp(TimesOp, VarExp("x"), VarExp("y")))) == IntValue(6))
  }

  "/" should "正しい値" in
  {
    assert(evalCV(Map(), Map(),
      IL.trans(BOpExp(DivideOp, IntExp(4), IntExp(1)))) == IntValue(4))
    assert(evalCV(Map(), Map("x"->IntValue(1)),
      IL.trans(BOpExp(DivideOp, IntExp(1), VarExp("x"))))== IntValue(1))
    assert(evalCV(Map(), Map("x"->IntValue(7), "y"->IntValue(3)),
      IL.trans(BOpExp(DivideOp, VarExp("x"), VarExp("y")))) == IntValue(2))
  }

  "::" should "正しい値" in
  {
    assert(evalCV(Map(), Map(),
      IL.trans(BOpExp(ConsOp, IntExp(1), NilExp)))
        == ListValue(List(1)))
    assert(evalCV(Map(), Map("x"->ListValue(List(2, 1))),
      IL.trans(BOpExp(ConsOp, IntExp(3), VarExp("x"))))
        == ListValue(List(3, 2, 1)))
  }

  "算術式" should "正しい値" in
  {
    assert(evalCV(Map(), Map("x"->IntValue(1),"y"->IntValue(2)),
      IL.trans(
        BOpExp(PlusOp,
          BOpExp(TimesOp, IntExp(1), VarExp("x")),
          BOpExp(TimesOp, VarExp("y"), VarExp("y")))))
          == IntValue(5))
  }

  "head" should "正しい値" in
  {
    assert(evalCV(Map(), Map(),
      IL.trans(
        UOpExp(HeadOp,
          BOpExp(ConsOp, IntExp(1), NilExp))))
      == IntValue(1))
    assert(evalCV(Map(), Map("x"->ListValue(List(5, 4, 3, 2, 1))),
      IL.trans(UOpExp(HeadOp, VarExp("x")))) == IntValue(5))
  }

    "tail" should "正しい値" in
  {
    assert(evalCV(Map(), Map(),
      IL.trans(
        UOpExp(TailOp,
          BOpExp(ConsOp, IntExp(1), NilExp))))
      == ListValue(List()))
    assert(evalCV(Map(), Map("x"->ListValue(List(5, 4, 3, 2, 1))),
      IL.trans(UOpExp(TailOp, VarExp("x")))) == ListValue(List(4, 3, 2, 1)))
  }

  "if" should "正しい値" in
  {
    assert(evalCV(Map(), Map("x"->ListValue(List()), "y"->IntValue(3)),
      IL.trans(IfExp(UOpExp(IsEmptyOp, VarExp("x")),
        BOpExp(PlusOp, IntExp(1), VarExp("y")),
        BOpExp(ConsOp, VarExp("y"), VarExp("x")))))
      == IntValue(4))
    assert(evalCV(Map(), Map("x"->ListValue(List(2, 1)), "y"->IntValue(3)),
      IL.trans(IfExp(UOpExp(IsEmptyOp, VarExp("x")),
        BOpExp(PlusOp, IntExp(1), VarExp("y")),
        BOpExp(ConsOp, VarExp("y"), VarExp("x")))))
      == ListValue(List(3, 2, 1)))
  }

  "関数適用" should "正しい値" in
  {
    val (c, v) = IL.trans(BOpExp(ConsOp, VarExp("x"), VarExp("y")))
    assert(
      evalCV(Map("f"->FValue(List("x","y"), c, v)),
        Map("x"->ListValue(List(2))),
        IL.trans(AppExp("f", List(IntExp(1), VarExp("x")))))== ListValue(List(1,2)))
  }

  "例: arith (x-y) * z" should "正しい値" in
  {
    val ds= Test.transFileDefs("examples/arith.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map(), IL.trans(AppExp("test", List(IntExp(4),IntExp(2),IntExp(3))))) == IntValue(6))
    assert(
      evalCV(fenv, Map("x"->IntValue(6), "y"->IntValue(3)),
        IL.trans(AppExp("test", List(VarExp("x"), VarExp("y"), BOpExp(PlusOp, VarExp("x"), VarExp("y"))))))
      == IntValue(27)
    )
  }


  "例: fact" should "正しい値" in
  {
    val ds = Test.transFileDefs("examples/fact.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map(), IL.trans(AppExp("fact", List(IntExp(4))))) == IntValue(24))
    assert(
      evalCV(fenv, Map("x"->IntValue(4)), IL.trans(AppExp("test", List(VarExp("x")))))
      == IntValue(24)
    )
  }

  "例: sort" should "正しい値" in
  {
    val ds = Test.transFileDefs("examples/sort.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map("x"->ListValue(List(3,2,1))), IL.trans(AppExp("sort", List(VarExp("x"))))) == ListValue(List(1,2,3)))
    assert(
      evalCV(fenv, Map(), IL.trans(AppExp("test", List())))
      == ListValue(List(1, 2, 3, 4, 5))
    )
  }

  "例: qsort" should "正しい値" in
  {
    val ds = Test.transFileDefs("examples/qsort.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map("x"->ListValue(List(3,2,1,3,5,3,1,4))), IL.trans(AppExp("qsort", List(VarExp("x"))))) == ListValue(List(1,1,2,3,3,3,4,5)))
  }

  "例: primes" should "正しい値" in
  {
    val ds = Test.transFileDefs("examples/primes.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map(), IL.trans(AppExp("primes", List(IntExp(20))))) ==
        ListValue(List(2,3,5,7,11,13,17,19)))
  }
}
