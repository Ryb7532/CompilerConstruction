object Main {
  def parseStr(s: String) =
    parseStrNonscala(s)

  def parseStrNonscala(s: String) = {
    val lexer = new Yylex (new java.io.StringReader(s))
    val parser = new ParserNonscala(lexer)
    parser.I()
  }

  def parseStrNonscalaDef(s: String) = {
    val lexer = new Yylex (new java.io.StringReader(s))
    val parser = new ParserNonscala(lexer)
    parser.D()
  }
}
