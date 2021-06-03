package nonscala

import org.scalatest._
import Base._
import Abssyn._
import Oper._
import Ty._
import TypeCheck._

class TypeCheckTest extends FlatSpec {
  "VarExp" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy), VarExp("x"))== IntTy)
    assert(tcheck(Map(), Map("x"->BoolTy), VarExp("x"))== BoolTy)
    assert(tcheck(Map(), Map("x"->IntListTy), VarExp("x"))== IntListTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map(), VarExp("x"))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("y"->IntTy), VarExp("x"))
    }
  }

  "IntExp" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map(), IntExp(1))== IntTy)
  }

  "NilExp" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map(), NilExp)== IntListTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy),
        BOpExp(ConsOp, NilExp, VarExp("x")))
    }
  }

  "isEmpty" should "正しい型付け" in
  {
    assert(
      tcheck(Map(), Map("x"->IntListTy), UOpExp(IsEmptyOp, VarExp("x")))== BoolTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntTy), UOpExp(IsEmptyOp, VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy), UOpExp(IsEmptyOp, VarExp("x")))
    }
  }

  "head" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntListTy), UOpExp(HeadOp, VarExp("x"))) == IntTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntTy), UOpExp(HeadOp, VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy), UOpExp(HeadOp, VarExp("x")))
    }
  }

  "tail" should "正しい型付け" in
  {
    assert(
      tcheck(Map(), Map("x"->IntListTy), UOpExp(TailOp, VarExp("x")))
      == IntListTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntTy), UOpExp(TailOp, VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy), UOpExp(TailOp, VarExp("x")))
    }
  }

  "+" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(PlusOp, IntExp(1), VarExp("x"))) == IntTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy),
        BOpExp(PlusOp, IntExp(1), VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(PlusOp, IntExp(1), VarExp("x")))
    }
  }

  "-" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(MinusOp, IntExp(1), VarExp("x"))) == IntTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy),
        BOpExp(MinusOp, IntExp(1), VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(MinusOp, IntExp(1), VarExp("x")))
    }
  }

  "*" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(TimesOp, IntExp(1), VarExp("x"))) == IntTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy),
        BOpExp(TimesOp, IntExp(1), VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(TimesOp, IntExp(1), VarExp("x")))
    }
  }

  "/" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(DivideOp, IntExp(1), VarExp("x"))) == IntTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy),
        BOpExp(DivideOp, IntExp(1), VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(DivideOp, IntExp(1), VarExp("x")))
    }

    // / by zero
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntTy),
        BOpExp(DivideOp, VarExp("x"), IntExp(0)))
    }
  }

  "==" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(EqOp, IntExp(1), VarExp("x"))) == BoolTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy),
        BOpExp(EqOp, IntExp(1), VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(EqOp, IntExp(1), VarExp("x")))
    }

    assert(
      tcheck(Map(), Map("x"->BoolTy, "y"->BoolTy),
        BOpExp(EqOp, VarExp("x"), VarExp("y")))
      == BoolTy)
    assert(
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(EqOp, BOpExp(EqOp, IntExp(1), IntExp(0)), VarExp("x")))
      == BoolTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy, "y"->IntTy),
        BOpExp(EqOp, VarExp("x"), VarExp("y")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy, "y"->IntListTy),
        BOpExp(EqOp, VarExp("x"), VarExp("y")))
    }

    assert(
      tcheck(Map(), Map("x"->IntListTy),
        BOpExp(EqOp, NilExp, VarExp("x"))) == BoolTy)
    assert(
      tcheck(Map(), Map("x"->IntListTy, "y"->IntListTy),
        BOpExp(EqOp, VarExp("x"), BOpExp(ConsOp, IntExp(1), VarExp("y")))) == BoolTy)
    assert(
      tcheck(Map(), Map("x"->IntListTy, "y"->IntTy),
        BOpExp(EqOp, VarExp("x"), BOpExp(ConsOp, IntExp(1), VarExp("y")))) == BoolTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy),
        BOpExp(EqOp, IntExp(1), VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy, "y"->BoolTy),
        BOpExp(EqOp, VarExp("x"), VarExp("y")))
    }
  }

  "<" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(LtOp, IntExp(1), VarExp("x"))) == BoolTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy),
        BOpExp(LtOp, IntExp(1), VarExp("x")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(LtOp, IntExp(1), VarExp("x")))
    }
  }

  "::" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map(), BOpExp(ConsOp, IntExp(1), NilExp)) == IntListTy)
    assert(tcheck(Map(), Map("x"->IntListTy),
      BOpExp(ConsOp, IntExp(1), VarExp("x"))) == IntListTy)

    assert(tcheck(Map(), Map(), BOpExp(ConsOp, IntExp(1), IntExp(2))) == IntListTy)
    assert(tcheck(Map(), Map("x"->IntTy),
        BOpExp(ConsOp, IntExp(1), VarExp("x"))) == IntListTy)

    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(ConsOp, IntExp(1), VarExp("x")))
    }

    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy, "y"->IntListTy),
        BOpExp(ConsOp, VarExp("x"), VarExp("y")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy, "y"->IntTy),
        BOpExp(ConsOp, VarExp("x"), VarExp("y")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy, "y"->IntListTy),
        BOpExp(ConsOp, VarExp("x"), VarExp("y")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy, "y"->IntTy),
        BOpExp(ConsOp, VarExp("x"), VarExp("y")))
    }

    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy, "y"->BoolTy),
        BOpExp(ConsOp, VarExp("x"), VarExp("y")))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntListTy, "y"->BoolTy),
        BOpExp(ConsOp, VarExp("x"), VarExp("y")))
    }
  }

  "if" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("e"->BoolTy, "x"->IntTy, "y"->IntTy),
      IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y")))) == IntTy)
    assert(tcheck(Map(), Map("e"->BoolTy, "x"->BoolTy, "y"->BoolTy),
      IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y")))) == BoolTy)
    assert(tcheck(Map(), Map("e"->BoolTy, "x"->IntListTy, "y"->IntListTy),
      IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y")))) == IntListTy)

    assertThrows[TypeError] {
      tcheck(Map(), Map("e"->IntTy, "x"->IntTy, "y"->IntTy),
        IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y"))))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("e"->IntListTy, "x"->IntTy, "y"->IntTy),
        IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y"))))
    }


    assertThrows[TypeError] {
      tcheck(Map(), Map("e"->BoolTy, "x"->IntTy, "y"->BoolTy),
        IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y"))))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("e"->BoolTy, "x"->IntTy, "y"->IntListTy),
        IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y"))))
    }

    assertThrows[TypeError] {
      tcheck(Map(), Map("e"->BoolTy, "x"->BoolTy, "y"->IntTy),
        IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y"))))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("e"->BoolTy, "x"->BoolTy, "y"->IntListTy),
        IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y"))))
    }

    assertThrows[TypeError] {
      tcheck(Map(), Map("e"->BoolTy, "x"->IntListTy, "y"->IntTy),
        IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y"))))
    }
    assertThrows[TypeError] {
      tcheck(Map(), Map("e"->BoolTy, "x"->IntListTy, "y"->BoolTy),
        IfExp(VarExp("e"), List(VarExp("x")), List(VarExp("y"))))
    }
  }

  "関数" should "正しい型付け" in
  {
    assert(
      tcheck(Map("f"->FuncTy(List(IntTy,BoolTy),IntListTy)),
        Map("x"->BoolTy),
        AppExp("f", List(IntExp(1), VarExp("x")))) == IntListTy)
    assert(
      tcheck(Map("f"->FuncTy(List(IntTy, IntListTy, BoolTy), IntTy)),
        Map("x"->IntListTy, "y"->BoolTy),
        AppExp("f", List(IntExp(1), VarExp("x"), VarExp("y")))) == IntTy)
    assert(
      tcheck(Map("f"->FuncTy(List(), IntTy)),
        Map(),
        AppExp("f", List())) == IntTy)
    assertThrows[TypeError] {
      tcheck(Map("f"->FuncTy(List(IntTy, BoolTy), IntListTy)),
        Map("x"->BoolTy),
        AppExp("f", List(VarExp("x"), IntExp(0))))
    }
  }

  "examples/sort" should "正しい型付け" in
  {
    val ds = Main.parseFileDefs("examples/sort.scala")
    tcheckDefs(ds)
    succeed
  }

  //拡張
  "val" should "NoneTy" in
  {
    assert(tcheck(Map(), Map(),
      ValExp("x", IntExp(1))) == NoneTy)
    assertThrows[TypeError] {
      bodytcheck(Map(), Map("x"->BoolTy),
        List(ValExp("x", IntExp(1)), VarExp("x")), NoneTy)
    }
  }

  "val" should "update env" in
  {
    assert(bodytcheck(Map(), Map(),
      List(ValExp("x", IntExp(1)), VarExp("x")), NoneTy) == IntTy)
    assert(bodytcheck(Map(), Map(),
      List(ValExp("x", NilExp), VarExp("x")), NoneTy) == IntListTy)
    assert(bodytcheck(Map(), Map(),
      List(ValExp("x", UOpExp(IsEmptyOp, NilExp)), VarExp("x")), NoneTy) == BoolTy)
  }

  "examples/append" should "" in
  {
    val ds = Main.parseFileDefs("examples/append.scala")
    tcheckDefs(ds)
    succeed
  }
}
