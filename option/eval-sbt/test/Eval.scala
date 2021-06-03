package nonscala

import org.scalatest._
import Base._
import Abssyn._
import Oper._
import Eval._

class EvalTest extends FlatSpec {
  "Var" should "正しい値" in
  {
    assert(eval(Map(), Map("x"->IntVal(1)), VarExp("x")) == IntVal(1))
    assert(eval(Map(), Map("x"->BoolVal(true)), VarExp("x")) == BoolVal(true))
    assert(eval(Map(), Map("x"->ListVal(List(1, 2))), VarExp("x"))
      == ListVal(List(1, 2)))
    assertThrows[EvalError] {
      eval(Map(), Map(), VarExp("x"))
    }
  }

  "Int" should "正しい値" in
  {
    assert(eval(Map(), Map(), IntExp(1)) == IntVal(1))
  }

  "Nil" should "正しい値" in
  {
    assert(eval(Map(), Map(), NilExp) == ListVal(Nil))
  }

  "isEmpty" should "正しい値" in
  {
    assert(eval(Map(), Map("x"->ListVal(Nil)),
      UOpExp(IsEmptyOp, VarExp("x")))== BoolVal(true))
    assert(eval(Map(), Map("x"->ListVal(List(1,2))),
      UOpExp(IsEmptyOp, VarExp("x"))) == BoolVal(false))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->IntVal(0)), UOpExp(IsEmptyOp, VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), UOpExp(IsEmptyOp, VarExp("x")))
    }
  }

  "head" should "正しい値" in
  {
    assert(eval(Map(), Map("x"->ListVal(List(1,2))),
      UOpExp(HeadOp, VarExp("x"))) == IntVal(1))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(Nil)), UOpExp(HeadOp, VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->IntVal(0)), UOpExp(HeadOp, VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), UOpExp(HeadOp, VarExp("x")))
    }
  }

  "tail" should "正しい値" in
  {
    assert(eval(Map(), Map("x"->ListVal(List(1,2))),
      UOpExp(TailOp, VarExp("x"))) == ListVal(List(2)))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(Nil)), UOpExp(TailOp, VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->IntVal(0)), UOpExp(TailOp, VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), UOpExp(TailOp, VarExp("x")))
    }
  }

  "+" should "正しい値" in
  {
    assert(eval(Map(), Map(), BOpExp(PlusOp, IntExp(1), IntExp(2)))
      == IntVal(3))
    assert(eval(Map(), Map("x"->IntVal(1)),
      BOpExp(PlusOp, IntExp(1), VarExp("x")))== IntVal(2))
    assert(eval(Map(), Map("x"->IntVal(1), "y"->IntVal(2)),
      BOpExp(PlusOp, VarExp("x"), VarExp("y"))) == IntVal(3))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(List(1, 2))), BOpExp(PlusOp, IntExp(1), VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), BOpExp(PlusOp, IntExp(1), VarExp("x")))
    }
  }

  "-" should "正しい値" in
  {
    assert(eval(Map(), Map(), BOpExp(MinusOp, IntExp(2), IntExp(1)))
      == IntVal(1))
    assert(eval(Map(), Map("x"->IntVal(1)),
      BOpExp(MinusOp, IntExp(1), VarExp("x")))== IntVal(0))
    assert(eval(Map(), Map("x"->IntVal(2), "y"->IntVal(1)),
      BOpExp(MinusOp, VarExp("x"), VarExp("y"))) == IntVal(1))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(List(1, 2))), BOpExp(MinusOp, IntExp(1), VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), BOpExp(MinusOp, IntExp(1), VarExp("x")))
    }
  }

  "*" should "正しい値" in
  {
    assert(eval(Map(), Map(), BOpExp(TimesOp, IntExp(1), IntExp(2)))
      == IntVal(2))
    assert(eval(Map(), Map("x"->IntVal(3)),
      BOpExp(TimesOp, IntExp(2), VarExp("x")))== IntVal(6))
    assert(eval(Map(), Map("x"->IntVal(3), "y"->IntVal(4)),
      BOpExp(TimesOp, VarExp("x"), VarExp("y"))) == IntVal(12))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(List(1, 2))), BOpExp(TimesOp, IntExp(1), VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), BOpExp(TimesOp, IntExp(1), VarExp("x")))
    }
  }

  "/" should "正しい値" in
  {
    assert(eval(Map(), Map(), BOpExp(DivideOp, IntExp(2), IntExp(1)))
      == IntVal(2))
    assert(eval(Map(), Map("x"->IntVal(2)),
      BOpExp(DivideOp, IntExp(4), VarExp("x")))== IntVal(2))
    assert(eval(Map(), Map("x"->IntVal(7), "y"->IntVal(3)),
      BOpExp(DivideOp, VarExp("x"), VarExp("y"))) == IntVal(2))

    assertThrows[EvalError] {
      eval(Map(), Map(), BOpExp(DivideOp, IntExp(3), IntExp(0)))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->IntVal(0)), BOpExp(DivideOp, IntExp(3), VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(List(1, 2))), BOpExp(DivideOp, IntExp(1), VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), BOpExp(DivideOp, IntExp(1), VarExp("x")))
    }
  }

  "==" should "正しい値" in
  {
    assert(eval(Map(), Map(), BOpExp(EqOp, IntExp(1), IntExp(1)))
      == BoolVal(true))
    assert(eval(Map(), Map(), BOpExp(EqOp, IntExp(1), IntExp(0)))
      == BoolVal(false))
    assert(eval(Map(), Map("x"->IntVal(1)),
      BOpExp(EqOp, IntExp(1), VarExp("x")))== BoolVal(true))
    assert(eval(Map(), Map("x"->IntVal(2)),
      BOpExp(EqOp, IntExp(1), VarExp("x")))== BoolVal(false))
    assert(eval(Map(), Map("x"->IntVal(0), "y"->IntVal(0)),
      BOpExp(EqOp, VarExp("x"), VarExp("y"))) == BoolVal(true))
    assert(eval(Map(), Map("x"->IntVal(0), "y"->IntVal(3)),
      BOpExp(EqOp, VarExp("x"), VarExp("y"))) == BoolVal(false))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(List(1))), BOpExp(EqOp, IntExp(2), VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), BOpExp(EqOp, IntExp(2), VarExp("x")))
    }
  }

  "<" should "正しい値" in
  {
    assert(eval(Map(), Map(), BOpExp(LtOp, IntExp(1), IntExp(2)))
      == BoolVal(true))
    assert(eval(Map(), Map(), BOpExp(LtOp, IntExp(1), IntExp(0)))
      == BoolVal(false))
    assert(eval(Map(), Map("x"->IntVal(3)),
      BOpExp(LtOp, IntExp(1), VarExp("x")))== BoolVal(true))
    assert(eval(Map(), Map("x"->IntVal(1)),
      BOpExp(LtOp, IntExp(1), VarExp("x")))== BoolVal(false))
    assert(eval(Map(), Map("x"->IntVal(0), "y"->IntVal(2)),
      BOpExp(LtOp, VarExp("x"), VarExp("y"))) == BoolVal(true))
    assert(eval(Map(), Map("x"->IntVal(2), "y"->IntVal(1)),
      BOpExp(LtOp, VarExp("x"), VarExp("y"))) == BoolVal(false))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(List(1))), BOpExp(LtOp, IntExp(2), VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), BOpExp(LtOp, IntExp(2), VarExp("x")))
    }
  }

  "::" should "正しい値" in
  {
    assert(eval(Map(), Map(), BOpExp(ConsOp, IntExp(1), BOpExp(ConsOp, IntExp(0), NilExp)))
      == ListVal(List(1, 0)))
    assert(eval(Map(), Map("x"->ListVal(List(2, 3))),
      BOpExp(ConsOp, IntExp(1), VarExp("x")))== ListVal(List(1, 2, 3)))
    assert(eval(Map(), Map("x"->IntVal(1)),
      BOpExp(ConsOp, VarExp("x"), NilExp))== ListVal(List(1)))
    assert(eval(Map(), Map("x"->IntVal(0), "y"->ListVal(List(2))),
      BOpExp(ConsOp, VarExp("x"), VarExp("y"))) == ListVal(List(0,2)))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(List(1))), BOpExp(ConsOp, VarExp("x"), NilExp))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), BOpExp(ConsOp, VarExp("x"), NilExp))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->IntVal(1)), BOpExp(ConsOp, IntExp(2), VarExp("x")))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->BoolVal(true)), BOpExp(ConsOp, IntExp(1), VarExp("x")))
    }
  }

  "if" should "正しい値" in
  {
    assert(
      eval(Map(), Map("x"->BoolVal(true)), IfExp(VarExp("x"), List(IntExp(1)), List(IntExp(0))))
      == IntVal(1))
    assert(
      eval(Map(), Map("x"->BoolVal(false)), IfExp(VarExp("x"), List(IntExp(1)), List(IntExp(0))))
      == IntVal(0))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->IntVal(1)), IfExp(VarExp("x"), List(IntExp(2)), List(IntExp(3))))
    }
    assertThrows[EvalError] {
      eval(Map(), Map("x"->ListVal(List(0))), IfExp(VarExp("x"), List(IntExp(2)), List(IntExp(3))))
    }
  }

  "関数適用" should "正しい値" in
  {
    assert(
      eval(Map("f"->FValue(List("x","y"), List(BOpExp(ConsOp, VarExp("x"), VarExp("y"))))),
        Map("x"->ListVal(List(2))),
        AppExp("f", List(IntExp(1), VarExp("x"))))== ListVal(List(1,2)))
  }

  "examples/insert" should "正しい値" in
  {
    val ds = Main.parseFileDefs("examples/insert.scala")
    val fenv = defs2env(ds)
    assert(
      eval(fenv, Map("x"->IntVal(2), "y"->ListVal(List(1,3))),
      AppExp("insert", List(VarExp("x"), VarExp("y"))))
      == ListVal(List(1, 2, 3)))
  }

  "examples/sort" should "正しい値" in
  {
    val ds = Main.parseFileDefs("examples/sort.scala")
    val fenv = defs2env(ds)
    assert(
      eval(fenv, Map("x"->ListVal(List(3,2,1))), AppExp("sort", List(VarExp("x"))))
      == ListVal(List(1,2,3)))
    assert(
      eval(fenv, Map(), AppExp("test", List()))
      == ListVal(List(1, 2, 3, 4, 5))
    )
  }

  "examples/qsort" should "正しい値" in
  {
    val ds = Main.parseFileDefs("examples/qsort.scala")
    val fenv = defs2env(ds)
    assert(
      eval(fenv, Map("x"->ListVal(List(3,2,5,1,4))), AppExp("qsort", List(VarExp("x"))))
      == ListVal(List(1,2,3,4,5)))
  }

  "examples/primes" should "正しい値" in
  {
    val ds = Main.parseFileDefs("examples/primes.scala")
    val fenv = defs2env(ds)
    assert(
      eval(fenv, Map(), AppExp("primes", List(IntExp(20)))) ==
        ListVal(List(2,3,5,7,11,13,17,19)))
  }

//拡張
  "val" should "正しい値" in
  {
    assert(eval(Map(), Map(), ValExp("x", IntExp(1))) ==
      NoneVal)
    assert(bodyeval(Map(), Map(),
      List(ValExp("x", IntExp(1))), NoneVal) == NoneVal)
    assert(bodyeval(Map(), Map(),
      List(ValExp("x", IntExp(1)), VarExp("x")), NoneVal) == IntVal(1))
    assert(bodyeval(Map(), Map(),
      List(ValExp("x", NilExp), VarExp("x")), NoneVal) == ListVal(List()))
    assert(bodyeval(Map(), Map(),
      List(ValExp("x", UOpExp(IsEmptyOp, NilExp)), VarExp("x")), NoneVal) == BoolVal(true))
    assert(bodyeval(Map(), Map(),
      List(ValExp("x", IntExp(1)), VarExp("x")), NoneVal)
        == IntVal(1))

    assertThrows[EvalError] {
      eval(Map(), Map("x"->IntVal(1)), ValExp("x", IntExp(0)))
    }
  }

  "examples/append" should "正しい値" in
  {
    val ds = Main.parseFileDefs("examples/append.scala")
    val fenv = defs2env(ds)
    assert(
      eval(fenv, Map("x"->ListVal(List(1, 2, 3)), "y"->ListVal(List(4, 5))), AppExp("append", List(VarExp("x"), VarExp("y")))) == ListVal(List(1, 2, 3, 4, 5)))
  }
}
