import org.scalatest._
import Abssyn._
import Oper._

class ParserTest extends FlatSpec {
  "+" should "左に強く結合" in
  {
    assert(Main.parseStr("abc + 10 + 20") ==
      BOpExp(PlusOp, BOpExp(PlusOp, VarExp("abc"), IntExp(10)), IntExp(20)))
    assert(Main.parseStr("abc + 10 + 20 + 30") ==
      BOpExp(PlusOp, BOpExp(PlusOp, BOpExp(PlusOp, VarExp("abc"), IntExp(10)), IntExp(20)), IntExp(30)))
    assert(Main.parseStr("abc + 10 + 20") == Main.parseStr("(abc + 10) + 20"))
  }

  "-" should "左に強く結合" in
  {
    assert(Main.parseStr("abc - 10 - 20") ==
      BOpExp(MinusOp, BOpExp(MinusOp, VarExp("abc"), IntExp(10)), IntExp(20)))
    assert(Main.parseStr("abc - 10 - 20 - 30") ==
      BOpExp(MinusOp, BOpExp(MinusOp, BOpExp(MinusOp, VarExp("abc"), IntExp(10)), IntExp(20)), IntExp(30)))
    assert(Main.parseStr("abc - 10 - 20") == Main.parseStr("(abc - 10) - 20"))
  }

  "*" should "左に強く結合" in
  {
    assert(Main.parseStr("abc * 10 * 20") ==
      BOpExp(TimesOp, BOpExp(TimesOp, VarExp("abc"), IntExp(10)), IntExp(20)))
    assert(Main.parseStr("abc * 10 * 20 * 30") ==
      BOpExp(TimesOp, BOpExp(TimesOp, BOpExp(TimesOp, VarExp("abc"), IntExp(10)), IntExp(20)), IntExp(30)))
    assert(Main.parseStr("abc * 10 * 20") == Main.parseStr("(abc * 10) * 20"))

  }

  "/" should "左に強く結合" in
  {
    assert(Main.parseStr("abc / 10 / 20") ==
      BOpExp(DivideOp, BOpExp(DivideOp, VarExp("abc"), IntExp(10)), IntExp(20)))
    assert(Main.parseStr("abc / 10 / 20 / 30") ==
      BOpExp(DivideOp, BOpExp(DivideOp, BOpExp(DivideOp, VarExp("abc"), IntExp(10)), IntExp(20)), IntExp(30)))
    assert(Main.parseStr("abc / 10 / 20") == Main.parseStr("(abc / 10) / 20"))
  }

  "()" should "正しく結合" in
  {
    assert(Main.parseStr("abc + (10 + 20)") ==
      BOpExp(PlusOp, VarExp("abc"), BOpExp(PlusOp, IntExp(10), IntExp(20))))
    assert(Main.parseStr("abc - (10 - 20)") ==
      BOpExp(MinusOp, VarExp("abc"), BOpExp(MinusOp, IntExp(10), IntExp(20))))
    assert(Main.parseStr("abc * (10 * 20)") ==
      BOpExp(TimesOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))))
    assert(Main.parseStr("abc / (10 / 20)") ==
      BOpExp(DivideOp, VarExp("abc"), BOpExp(DivideOp, IntExp(10), IntExp(20))))
    assert(Main.parseStr("abc * (10 + 20)") ==
      BOpExp(TimesOp, VarExp("abc"), BOpExp(PlusOp, IntExp(10), IntExp(20))))
    assert(Main.parseStr("(abc + 10) * ((20 - xyz) / 30)") ==
      BOpExp(TimesOp, BOpExp(PlusOp, VarExp("abc"), IntExp(10)), BOpExp(DivideOp, BOpExp(MinusOp, IntExp(20), VarExp("xyz")), IntExp(30))))
  }

  "*,+" should "*の方が強く結合" in
  {
    assert(Main.parseStr("abc + 10 * 20") ==
      BOpExp(PlusOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))))
    assert(Main.parseStr("abc * 10 + xyz * 20") ==
      BOpExp(PlusOp, BOpExp(TimesOp, VarExp("abc"), IntExp(10)), BOpExp(TimesOp, VarExp("xyz"), IntExp(20))))
    assert(Main.parseStr("abc * 10 + xyz * 20") == Main.parseStr("(abc * 10) + (xyz * 20)"))
  }

  "+,-,*,/" should "*,/の方が強く結合" in
  {
    assert(Main.parseStr("abc + 10 * 20 - xyz / 30") ==
      BOpExp(MinusOp, BOpExp(PlusOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))), BOpExp(DivideOp, VarExp("xyz"), IntExp(30))))
    assert(Main.parseStr("abc + 10 * 20 - xyz / 30") == Main.parseStr("(abc + (10 * 20)) - (xyz / 30)"))
  }

  "カッコの中に*と+" should "上の規則を適用" in
  {
    assert(Main.parseStr("(abc + 10 * 20)") ==
      BOpExp(PlusOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))))
    assert(Main.parseStr("(abc * 10 + xyz * 20)") ==
      BOpExp(PlusOp, BOpExp(TimesOp, VarExp("abc"), IntExp(10)), BOpExp(TimesOp, VarExp("xyz"), IntExp(20))))
    assert (Main.parseStr("(abc + 10 * 20)") == Main.parseStr("abc + 10 * 20"))
    assert(Main.parseStr("(abc * 10 + xyz * 20)") == Main.parseStr("abc * 10 + xyz * 20"))
  }

  "算術式" should "項としてNilを含む" in
  {
    assert(Main.parseStr("abc + 10 + Nil") ==
      BOpExp(PlusOp, BOpExp(PlusOp, VarExp("abc"), IntExp(10)), NilExp))
  }

  "::" should "右に強く結合" in
  {
    assert(Main.parseStr("10 :: 20 :: Nil") ==
      BOpExp(ConsOp, IntExp(10), BOpExp(ConsOp, IntExp(20), NilExp)))
    assert(Main.parseStr("10 :: 20 :: xyz :: Nil") ==
      BOpExp(ConsOp, IntExp(10), BOpExp(ConsOp, IntExp(20), BOpExp(ConsOp, VarExp("xyz"), NilExp))))
  }

  "List" should "最後の要素はNilでなくても良い" in
  {
    assert(Main.parseStr("10 :: 20 :: xyz") ==
      BOpExp(ConsOp, IntExp(10), BOpExp(ConsOp, IntExp(20), VarExp("xyz"))))
  }

  "List" should "途中の要素にNil" in
  {
    assert(Main.parseStr("10 :: Nil :: 20 :: xyz") ==
      BOpExp(ConsOp, IntExp(10), BOpExp(ConsOp, NilExp, BOpExp(ConsOp, IntExp(20), VarExp("xyz")))))
  }

  "::と算術式" should "算術式を優先" in
  {
    assert(Main.parseStr("5 :: 10 * 20 :: Nil") ==
      BOpExp(ConsOp, IntExp(5),
        BOpExp(ConsOp, BOpExp(TimesOp, IntExp(10), IntExp(20)), NilExp)))
    assert(Main.parseStr("5 :: abc + 10 * 20 :: Nil") ==
      BOpExp(ConsOp, IntExp(5),
        BOpExp(ConsOp, BOpExp(PlusOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))), NilExp)))
  }

  "if文と==" should "" in
  {
    assert(Main.parseStr("if (10 == 20) x else y") ==
      IfExp(BOpExp(EqOp, IntExp(10), IntExp(20)), VarExp("x"), VarExp("y")))
    assert(Main.parseStr("if (10 + abc == 20) x * 5 else y") ==
      IfExp(BOpExp(EqOp, BOpExp(PlusOp, IntExp(10), VarExp("abc")), IntExp(20)), BOpExp(TimesOp, VarExp("x"), IntExp(5)), VarExp("y")))
  }

  "if文と<" should "" in
  {
    assert(Main.parseStr("if (10 < 20) x else y") ==
      IfExp(BOpExp(LtOp, IntExp(10), IntExp(20)), VarExp("x"), VarExp("y")))
    assert(Main.parseStr("if (10 + abc < 20) x * 5 else y") ==
      IfExp(BOpExp(LtOp, BOpExp(PlusOp, IntExp(10), VarExp("abc")), IntExp(20)), BOpExp(TimesOp, VarExp("x"), IntExp(5)), VarExp("y")))
  }

  "if文のネスト" should "" in
  {
    assert(Main.parseStr("if (10 == 20) x else if (x == y) 1 else 2") ==
    IfExp(BOpExp(EqOp, IntExp(10), IntExp(20)), VarExp("x"),
      IfExp(BOpExp(EqOp, VarExp("x"), VarExp("y")), IntExp(1), IntExp(2))))
    assert(Main.parseStr("if (10 == 20) x else if (x == y) 1 else if (abc + 10 < 20) Nil else 2") ==
    IfExp(BOpExp(EqOp, IntExp(10), IntExp(20)), VarExp("x"),
      IfExp(BOpExp(EqOp, VarExp("x"), VarExp("y")), IntExp(1),
        IfExp(BOpExp(LtOp, BOpExp(PlusOp, VarExp("abc"), IntExp(10)), IntExp(20)), NilExp, IntExp(2)))))
  }
}
