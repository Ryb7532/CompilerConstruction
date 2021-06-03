import Base._
import Oper._
import Ty._
import Abssyn._
import Tokens._

class ParserNonscala (val src: Yylex) {

  var tok: Token = src.yylex()

  def advance () =  tok = src.yylex()

  def eat (t: Token) =
    if (tok == t) advance() else error()


  def F(): Exp =
    tok match {
      case NIL => advance(); NilExp
      case INT(i) => advance(); IntExp(i)
      case ID(s) => advance(); Fprime(s)
      case LPAREN =>
        {
          eat(LPAREN)
          val e = E()
          eat(RPAREN)
          e
        }
      case _ => error()
    }

  def Fprime(s: String): Exp =
    tok match {
      case LPAREN => {
        eat(LPAREN)
        val args = A()
        eat(RPAREN)
        AppExp(s, args)
      }
      case DOT => {
        eat(DOT)
        val uop = M()
        UOpExp(uop, VarExp(s))
      }
      case ELSE | DEF | RPAREN | COMMA | PLUS | MINUS | TIMES | DIV | COLONCOLON | EQEQ | LESS | EOF => VarExp(s)
      case _ => error()
    }

  def A(): List[Exp] =
    tok match {
      case ID(_) | INT(_) | NIL | LPAREN => C()::Aprime()
      case RPAREN => Nil
      case _ => error()
    }

  def Aprime(): List[Exp] =
    tok match {
      case COMMA => eat(COMMA); C()::Aprime()
      case RPAREN => Nil
      case _ => error()
    }

  def M(): UOp =
    tok match {
      case ID(s) => {
        if (s == "isEmpty") {
          advance()
          IsEmptyOp
        }
        else if (s == "head") {
          advance()
          HeadOp
        }
        else if (s == "tail") {
          advance()
          TailOp
        }
        else error()
      }
      case _ => error()
    }

  def T(): Exp =
    tok match {
      case ID(_) | INT(_) | NIL | LPAREN =>  Tprime(F())
      case _ => error()
    }

  def Tprime(e: Exp): Exp =
    tok match {
      case TIMES => eat(TIMES); Tprime(BOpExp(TimesOp, e, F()))
      case DIV => eat(DIV); Tprime(BOpExp(DivideOp, e, F()))
      case ELSE | DEF | RPAREN | COMMA | PLUS | MINUS | DOT | COLONCOLON | EQEQ | LESS | EOF => e
      case _ => error()
    }

  def E(): Exp =
    tok match {
      case ID(_) | INT(_) | NIL | LPAREN => Eprime(T())
      case _ => error()
    }

  def Eprime(e: Exp): Exp =
    tok match {
      case PLUS => eat(PLUS); Eprime(BOpExp(PlusOp, e, T()))
      case MINUS => eat(MINUS); Eprime(BOpExp(MinusOp, e, T()))
      case ELSE | DEF | RPAREN | COMMA | DOT | COLONCOLON | EQEQ | LESS | EOF => e
      case _ => error()
    }

  def C(): Exp =
    tok match {
      case ID(_) | INT(_) | NIL | LPAREN => Cprime(E())
      case _ => error()
    }

  def Cprime(e: Exp): Exp =
    tok match {
      case COLONCOLON => eat(COLONCOLON); BOpExp(ConsOp, e, C())
      case ELSE | DEF | RPAREN | COMMA | DOT | EOF => e
      case _ => error()
    }

  def B(): Exp =
    tok match {
      case ID(_) | INT(_) | NIL | LPAREN => Bprime(E())
      case _ => error()
    }

  def Bprime(e: Exp): Exp =
    tok match {
      case RPAREN => e
      case EQEQ => eat(EQEQ); BOpExp(EqOp, e, E())
      case LESS => eat(LESS); BOpExp(LtOp, e, E())
      case _ => error()
    }

  def I(): Exp =
    tok match {
      case ID(_) | INT(_) | NIL | LPAREN => C()
      case IF => {
        eat(IF)
        eat(LPAREN)
        val e = B()
        eat(RPAREN)
        val e1 = I()
        eat(ELSE)
        val e2 = I()
        IfExp(e, e1, e2)
      }
      case _ => error()
    }

  def U(): Ty =
    tok match {
      case ID(s) => {
        if (s == "Int") {
          advance()
          IntTy
        }
        else if (s == "Boolean") {
          advance()
          BoolTy
        }
        else if (s == "List") {
          advance()
          eat(LBRACKET)
          tok match {
            case ID(t) => {
              if (t == "Int") {
                advance()
                eat(RBRACKET)
                IntListTy
              }
              else error()
            }
            case _ => error()
          }
        } else error()
      }
      case _ => error()
    }

  def DAr(): List[(Var, Ty)] =
    tok match {
      case ID(v) => {
        advance()
        eat(COLON)
        val t = U()
        (v, t)::DArprime()
      }
      case _ => error()
    }

  def DArprime(): List[(Var, Ty)] =
    tok match {
      case COMMA => eat(COMMA); DAr()
      case RPAREN => Nil
      case _ => error()
    }

  def DA(): List[(Var, Ty)] =
    tok match {
      case ID(_) => DAr()
      case RPAREN => Nil
      case _ => error()
    }

  def D(): Def =
    tok match {
      case DEF => {
        eat(DEF)
        tok match {
          case ID(name) => {
            advance()
            eat(LPAREN)
            val args = DA()
            eat(RPAREN)
            eat(COLON)
            val rtype = U()
            eat(EQ)
            val body = I()
            Def(name, args, rtype, body)
          }
          case _ => error()
        }
      }
      case _ => error()
    }

  def Ds(): List[Def] = {
    tok match {
      case DEF => {
        val d = D()
        val ds = Ds()
        d::ds
      }
      case EOF => Nil
      case _ => error()
    }
  }
}

