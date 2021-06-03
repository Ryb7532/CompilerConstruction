import Base._
import Oper._
import Ty._
import Abssyn._
import Tokens._

class ParserNonscala (val src: Yylex) {

  var tok: Token = src.yylex()

  def advance () =  tok = src.yylex()

  def eat (t: Token) = {
    if (tok == t) advance()
    else error()
  }
/*
  def eatBreak() = {
    while (tok == BREAK) advance()
  }*/


  def F(): Exp =
    tok match {
      case NIL => advance(); NilExp
      case INT(i) => advance(); IntExp(i)
      case ID(s) => advance(); Fprime(s)
      case IDBR(s) => advance(); VarExp(s)
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
      case VAL | ID(_) | IDBR(_) | INT(_) | NIL | IF | ELSE | DEF | RPAREN | RCURLY | COMMA | PLUS | MINUS | TIMES | DIV | COLONCOLON | EQEQ | LESS |/* BREAK |*/ EOF => {
        //eatBreak()
        VarExp(s)
      }
      case _ => error()
    }

  def A(): List[Exp] =
    tok match {
      case ID(_) | IDBR(_) | INT(_) | NIL | LPAREN => C()::Aprime()
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
          //eatBreak()
          IsEmptyOp
        }
        else if (s == "head") {
          advance()
          //eatBreak()
          HeadOp
        }
        else if (s == "tail") {
          advance()
          //eatBreak()
          TailOp
        }
        else error()
      }
      case IDBR(s) => {
        if (s == "isEmpty") {
          advance()
          //eatBreak()
          IsEmptyOp
        }
        else if (s == "head") {
          advance()
          //eatBreak()
          HeadOp
        }
        else if (s == "tail") {
          advance()
          //eatBreak()
          TailOp
        }
        else error()
      }
      case _ => error()
    }

  def T(): Exp =
    tok match {
      case ID(_) | IDBR(_) | INT(_) | NIL | LPAREN =>  Tprime(F())
      case _ => error()
    }

  def Tprime(e: Exp): Exp =
    tok match {
      case TIMES => eat(TIMES); Tprime(BOpExp(TimesOp, e, F()))
      case DIV => eat(DIV); Tprime(BOpExp(DivideOp, e, F()))
      case VAL | IF | ID(_) | IDBR(_) | INT(_) | NIL | LPAREN | ELSE | DEF | RPAREN | RCURLY | COMMA | PLUS | MINUS | DOT | COLONCOLON | EQEQ | LESS | EOF => e
      case _ => error()
    }

  def E(): Exp =
    tok match {
      case ID(_) | IDBR(_) | INT(_) | NIL | LPAREN => Eprime(T())
      case _ => error()
    }

  def Eprime(e: Exp): Exp =
    tok match {
      case PLUS => eat(PLUS); Eprime(BOpExp(PlusOp, e, T()))
      case MINUS => eat(MINUS); Eprime(BOpExp(MinusOp, e, T()))
      case VAL | IF | ID(_) | IDBR(_) | INT(_) | NIL | LPAREN | ELSE | DEF | RPAREN | RCURLY | COMMA | DOT | COLONCOLON | EQEQ | LESS | EOF => e
      case _ => error()
    }

  def C(): Exp =
    tok match {
      case ID(_) | IDBR(_) | INT(_) | NIL | LPAREN => Cprime(E())
      case _ => error()
    }

  def Cprime(e: Exp): Exp =
    tok match {
      case COLONCOLON => eat(COLONCOLON); BOpExp(ConsOp, e, C())
      case VAL | IF | ID(_) | IDBR(_) | INT(_) | NIL | LPAREN | ELSE | DEF | RPAREN | RCURLY | COMMA | DOT | EOF => e
      case _ => error()
    }

  def B(): Exp =
    tok match {
      case ID(_) | IDBR(_) | INT(_) | NIL | LPAREN => Bprime(E())
      case _ => error()
    }

  def Bprime(e: Exp): Exp =
    tok match {
      case RPAREN => e
      case EQEQ => eat(EQEQ); BOpExp(EqOp, e, E())
      case LESS => eat(LESS); BOpExp(LtOp, e, E())
      case _ => error()
    }

  def V(): Exp = {
    tok match {
      case VAL => {
        eat(VAL)
        tok match {
          case ID(n) => {
            advance()
            //eatBreak()
            eat(EQ)
            ValExp(n, C())
          }
          case _ => error()
        }
      }
      case ID(_) | IDBR(_) | INT(_) | NIL | LPAREN => C()
      case _ => error()
    }
  }

  def I(): Exp = {
    tok match {
      case ID(_) | IDBR(_) | INT(_) | NIL | LPAREN | VAL => V()
      case IF => {
        eat(IF)
        eat(LPAREN)
        val e = B()
        eat(RPAREN)
        val e1 = BD()
        eat(ELSE)
        val e2 = BD()
        IfExp(e, e1, e2)
      }
      case _ => error()
    }
  }

  def Iprime(): List[Exp] = {
    tok match {
      case IF | ID(_) | INT(_) | NIL | LPAREN | VAL => BDprime()
      case RCURLY => Nil
      case _ => error()
    }
  }

  def BD(): List[Exp] = {
    tok match {
      case LCURLY => {
        eat(LCURLY)
        val a = BDprime()
        eat(RCURLY)
        a
      }
      case IF | ID(_) | IDBR(_) | INT(_) | NIL | LPAREN | VAL => I()::Nil
      case _ => error()
    }
  }

  def BDprime(): List[Exp] = {
    I()::Iprime()
  }

  def U(): Ty =
    tok match {
      case ID(s) => {
        if (s == "Int") {
          advance()
          //eatBreak()
          IntTy
        }
        else if (s == "Boolean") {
          advance()
          //eatBreak()
          BoolTy
        }
        else if (s == "List") {
          advance()
          //eatBreak()
          eat(LBRACKET)
          tok match {
            case ID(t) => {
              if (t == "Int") {
                advance()
                //eatBreak()
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
        //eatBreak()
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

  // ( DA()
  def DA(): List[(Var, Ty)] =
    tok match {
      case ID(_) => DAr()
      case RPAREN => Nil
      case _ => error()
    }

  def D(): Def = {
    eat(DEF)
    tok match {
      case ID(name) => {
        advance()
        //eatBreak()
        eat(LPAREN)
        val args = DA()
        eat(RPAREN)
        eat(COLON)
        val rtype = U()
        eat(EQ)
        val body = BD()
        Def(name, args, rtype, body)
      }
      case _ => error()
    }
  }

  def Ds(): List[Def] = {
    //eatBreak()
    tok match {
      case DEF => {
        val d = D()
        val ds = Ds()
        d::ds
      }
      case EOF => Nil
    }
  }
}

