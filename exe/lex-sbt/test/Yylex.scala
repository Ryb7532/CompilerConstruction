import org.scalatest._
import java.io.StringReader
import Tokens._

class YylexTest extends FlatSpec {
  "10" should "整数" in
  {
    val l = new Yylex(new StringReader("10"))

    assert(l.yylex() == NUM(10))
  }

  "0" should "整数" in
  {
    val l = new Yylex(new StringReader("0"))

    assert(l.yylex() == NUM(0))
  }

  "識別子" should "正しい字句解析" in
  {
    val l = new Yylex(new StringReader("ab0c1d"))

    assert(l.yylex() == ID("ab0c1d"))
  }

  "トークンif" should "正しい字句解析" in
  {
    val l = new Yylex(new StringReader("if"))

    assert(l.yylex() == IF)
  }


  "if0" should "識別子" in
  {
    val l = new Yylex(new StringReader("if0"))

    assert(l.yylex() == ID("if0"))
  }

  "浮動小数点数 12.34" should "正しい字句解析" in
  {
    val l = new Yylex(new StringReader("12.34"))

    assert(l.yylex() == REAL(12.34))
  }

  "浮動小数点数 .1234" should  "正しい字句解析" in
  {
    val l = new Yylex(new StringReader(".1234"))

    assert(l.yylex() == REAL(0.1234))
  }

  "浮動小数点数 1234." should "正しい字句解析" in
  {
    val l = new Yylex(new StringReader("1234."))

    assert(l.yylex() == REAL(1234.0))
  }

  // ここから課題のテスト

  "`if`" should "識別子" in
  {
    val l = new Yylex(new StringReader("`if`"))

    assert(l.yylex() == ID("if"))
  }

  "`abc`" should "識別子" in
  {
    val l = new Yylex(new StringReader("`abc`"))

    assert(l.yylex() == ID("abc"))
  }

  "`if0`" should "識別子" in
  {
    val l = new Yylex(new StringReader("`if0`"))

    assert(l.yylex() == ID("if0"))
  }
    
    
  "012" should "エラー" in
  {
    val l = new Yylex(new StringReader("012"))
    assertThrows[Base.MyError] {l.yylex()}
  }

}
