import org.scalatest._
import Oper._
import Ty._
import Abssyn._

class ParserNonscalaTest extends FlatSpec {
  def parseStr(s: String) = {
    val lexer = new Yylex (new java.io.StringReader(s))
    val parser = new ParserNonscala(lexer)
    parser.I()
  }

  def parseStrVal(s: String) = {
    val lexer = new Yylex (new java.io.StringReader(s))
    val parser = new ParserNonscala(lexer)
    parser.BD()
  }

  def parseStrDef(s: String) = {
    val lexer = new Yylex (new java.io.StringReader(s))
    val parser = new ParserNonscala(lexer)
    parser.D()
  }

  def parseFileDef(s: String) = {
    val f = new java.io.File(s)
    val lexer = new Yylex (new java.io.FileReader(f))
    val parser = new ParserNonscala(lexer)
    parser.Ds()
  }




  "+" should "左に強く結合" in
  {
    assert(Main.parseStr("abc + 10 + 20") ==
      BOpExp(PlusOp, BOpExp(PlusOp, VarExp("abc"), IntExp(10)), IntExp(20)))
    assert(Main.parseStr("abc + 10 + 20 + 30") ==
      BOpExp(PlusOp, BOpExp(PlusOp, BOpExp(PlusOp, VarExp("abc"), IntExp(10)), IntExp(20)), IntExp(30)))
    assert(Main.parseStr("abc + 10 + 20 + 30") == Main.parseStr("((abc + 10) + 20) + 30"))
  }

  "-" should "左に強く結合" in
  {
    assert(Main.parseStr("abc - 10 - 20") ==
      BOpExp(MinusOp, BOpExp(MinusOp, VarExp("abc"), IntExp(10)), IntExp(20)))
    assert(Main.parseStr("abc - 10 - 20 - 30") ==
      BOpExp(MinusOp, BOpExp(MinusOp, BOpExp(MinusOp, VarExp("abc"), IntExp(10)), IntExp(20)), IntExp(30)))
    assert(Main.parseStr("abc - 10 - 20 - 5") == Main.parseStr("((abc - 10) - 20) - 5"))
  }

  "*" should "左に強く結合" in
  {
    assert(Main.parseStr("abc * 10 * 20") ==
      BOpExp(TimesOp, BOpExp(TimesOp, VarExp("abc"), IntExp(10)), IntExp(20)))
    assert(Main.parseStr("abc * 10 * 20 * 30") ==
      BOpExp(TimesOp, BOpExp(TimesOp, BOpExp(TimesOp, VarExp("abc"), IntExp(10)), IntExp(20)), IntExp(30)))
    assert(Main.parseStr("abc * 10 * 20 * 30") == Main.parseStr("((abc * 10) * 20) * 30"))

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
      IfExp(BOpExp(EqOp, IntExp(10), IntExp(20)), List(VarExp("x")), List(VarExp("y"))))
    assert(Main.parseStr("if (10 + abc == 20) x * 5 else y") ==
      IfExp(BOpExp(EqOp, BOpExp(PlusOp, IntExp(10), VarExp("abc")), IntExp(20)), List(BOpExp(TimesOp, VarExp("x"), IntExp(5))), List(VarExp("y"))))
  }

  "if文と<" should "" in
  {
    assert(Main.parseStr("if (10 < 20) x else y") ==
      IfExp(BOpExp(LtOp, IntExp(10), IntExp(20)), List(VarExp("x")), List(VarExp("y"))))
    assert(Main.parseStr("if (10 + abc < 20) x * 5 else y") ==
      IfExp(BOpExp(LtOp, BOpExp(PlusOp, IntExp(10), VarExp("abc")), IntExp(20)), List(BOpExp(TimesOp, VarExp("x"), IntExp(5))), List(VarExp("y"))))
  }

  "if文のネスト" should "" in
  {
    assert(Main.parseStr("if (10 == 20) x else if (x == y) 1 else 2") ==
    IfExp(BOpExp(EqOp, IntExp(10), IntExp(20)), List(VarExp("x")),
      List(IfExp(BOpExp(EqOp, VarExp("x"), VarExp("y")), List(IntExp(1)), List(IntExp(2))))))
    assert(Main.parseStr("if (10 == 20) x else if (x == y) 1 else if (abc + 10 < 20) Nil else 2") ==
      IfExp(BOpExp(EqOp, IntExp(10), IntExp(20)), List(VarExp("x")),
      List(IfExp(BOpExp(EqOp, VarExp("x"), VarExp("y")), List(IntExp(1)),
        List(IfExp(BOpExp(LtOp, BOpExp(PlusOp, VarExp("abc"), IntExp(10)), IntExp(20)), List(NilExp), List(IntExp(2))))))))
  }

  "isEmpty" should "" in
  {
    assert(parseStr("x.isEmpty") == UOpExp(IsEmptyOp, VarExp("x")))
  }

  "head" should "" in
  {
    assert(parseStr("x.head") == UOpExp(HeadOp, VarExp("x")))
  }

  "tail" should "" in
  {
    assert(parseStr("x.tail") == UOpExp(TailOp, VarExp("x")))
  }

  "関数適用" should "" in
  {
    assert(parseStr("f(10,20)") == AppExp("f", List(IntExp(10), IntExp(20))))
    assert(parseStr("f()") == AppExp("f", List()))
    assert(parseStr("f(10, g(), abc + 20, x::Nil, y.head)") ==
      AppExp("f", List(
        IntExp(10), AppExp("g", List()), BOpExp(PlusOp, VarExp("abc"), IntExp(20)), BOpExp(ConsOp, VarExp("x"), NilExp), UOpExp(HeadOp, VarExp("y")))))
  }

  "関数定義" should "" in
  {
    assert(parseStrDef("def f(x:Int, y:Boolean): List[Int] = Nil") ==
      Def("f", List(("x", IntTy), ("y", BoolTy)), IntListTy, List(NilExp)))
    assert(parseStrDef("def f(): Int = 10") ==
      Def("f", List(), IntTy, List(IntExp(10))))
    assert(parseStrDef("def f(x:Int, y:Boolean): List[Int] = if (y) x::Nil else Nil") ==
      Def("f", List(("x", IntTy), ("y", BoolTy)), IntListTy,
        List(IfExp(VarExp("y"), List(BOpExp(ConsOp, VarExp("x"), NilExp)), List(NilExp)))))
  }

  "examples: insert" should "" in
  {
    assert(parseFileDef("examples/insert.scala") ==
      List(
        Def("insert",List(("x",IntTy), ("l",IntListTy)),IntListTy,
          List(IfExp(UOpExp(IsEmptyOp,VarExp("l")),
            List(BOpExp(ConsOp,VarExp("x"),NilExp)),
            List(IfExp(BOpExp(LtOp,VarExp("x"),UOpExp(HeadOp,VarExp("l"))),
              List(BOpExp(ConsOp,VarExp("x"),VarExp("l"))),
              List(BOpExp(ConsOp,UOpExp(HeadOp,VarExp("l")),
                AppExp("insert",List(VarExp("x"), UOpExp(TailOp,VarExp("l")))))))))))))
  }

  "examples: sort" should "" in
  {
    assert(parseFileDef("examples/sort.scala") ==
      List(
        Def(
          "insert",
          List(
            ("x",IntTy),
            ("l",IntListTy)
          ),
          IntListTy,
          List(IfExp(
            UOpExp(IsEmptyOp,VarExp("l")),
            List(BOpExp(ConsOp,VarExp("x"),NilExp)),
            List(IfExp(
              BOpExp(
                LtOp,
                VarExp("x"),
                UOpExp(HeadOp,VarExp("l"))
              ),
              List(BOpExp(ConsOp,VarExp("x"),VarExp("l"))),
              List(BOpExp(
                ConsOp,
                UOpExp(HeadOp,VarExp("l")),
                AppExp(
                  "insert",
                  List(
                    VarExp("x"),
                    UOpExp(TailOp,VarExp("l"))
                  )
                )
              ))
            ))
          ))
        ),
        Def(
          "sort",
          List(("l", IntListTy)),
          IntListTy,
          List(IfExp(
            UOpExp(IsEmptyOp, VarExp("l")),
            List(NilExp),
            List(AppExp(
              "insert",
              List(
                UOpExp(HeadOp, VarExp("l")),
                AppExp(
                  "sort",
                  List(
                    UOpExp(TailOp, VarExp("l"))
                  )
                )
              )
            ))
          ))
        ),
        Def(
          "test",
          List(("n", IntTy)),
          IntListTy,
          List(IfExp(
            BOpExp(
              EqOp, VarExp("n"), IntExp(0)
            ),
            List(NilExp),
            List(AppExp(
              "insert",
              List(
                VarExp("n"),
                AppExp(
                  "test",
                  List(
                    BOpExp(MinusOp, VarExp("n"), IntExp(1))
                  )
                )
              )
            ))
          ))
        )
      )
    )
  }

  //拡張
  "expressions" should "" in
  {
    assert(parseStrVal("{x y}") == List(VarExp("x"), VarExp("y")))
    assert(parseStrVal("{x\ny}") == List(VarExp("x"), VarExp("y")))
    assert(parseStrVal("{x;y}") == List(VarExp("x"), VarExp("y")))
  }

  "if" should "extended" in
  {
    assert(parseStrVal("{if (x) {y z+1} else y}") ==
      List(IfExp(VarExp("x"),
        List(VarExp("y"), BOpExp(PlusOp, VarExp("z"), IntExp(1))),
        List(VarExp("y")))))
    assert(parseStrVal("{if (x) {y;z+1} else y}") ==
      List(IfExp(VarExp("x"),
        List(VarExp("y"), BOpExp(PlusOp, VarExp("z"), IntExp(1))),
        List(VarExp("y")))))
  }

  "val" should "" in
  {
    assert(parseStrVal("{val x = 1}") == List(ValExp("x", IntExp(1))))
    assert(parseStrVal("{val x = 1 val y = 2}") ==
      List(ValExp("x", IntExp(1)), ValExp("y", IntExp(2))))
    assert(parseStrVal("{val x = 1;val y = 2}") ==
      List(ValExp("x", IntExp(1)), ValExp("y", IntExp(2))))
    assert(parseStrVal("{val x = 1\nval y = 2}") ==
      List(ValExp("x", IntExp(1)), ValExp("y", IntExp(2))))
    assert(parseStrVal("{val x = Nil}") == List(ValExp("x", NilExp)))
    assert(parseStrVal("{val x = 2::1}") ==
      List(ValExp("x", BOpExp(ConsOp, IntExp(2), IntExp(1)))))
    assert(parseStrVal("{val x = Nil x.isEmpty}") ==
      List(ValExp("x", NilExp), UOpExp(IsEmptyOp, VarExp("x"))))
    assert(parseStrVal("{val x = 1;if (x == 1) x+1 else x-1}") ==
      List(ValExp("x", IntExp(1)),
        IfExp(BOpExp(EqOp, VarExp("x"), IntExp(1)),
          List(BOpExp(PlusOp, VarExp("x"), IntExp(1))),
          List(BOpExp(MinusOp, VarExp("x"), IntExp(1))))))
    assert(parseStrVal("{if (x) {val y = 1 y} else 2}") ==
      List(
        IfExp(VarExp("x"), List(ValExp("y", IntExp(1)), VarExp("y")),
        List(IntExp(2)))))
  }

  "examples/append.scala" should "" in
  {
    assert(parseFileDef("examples/append.scala") ==
      List(
        Def(
          "append",
          List(
            ("x",IntListTy),
            ("y",IntListTy)
          ),
          IntListTy,
          List(IfExp(
            UOpExp(IsEmptyOp,VarExp("x")),
            List(VarExp("y")),
            List(
              ValExp("h", UOpExp(HeadOp, VarExp("x"))),
              ValExp("t", UOpExp(TailOp, VarExp("x"))),
              BOpExp(
                ConsOp,
                VarExp("h"),
                AppExp(
                  "append",
                  List(
                    VarExp("t"),
                    VarExp("y")
                  )
                )
              )
            )
          ))
        )
      )
    )
  }
}
