import org.scalatest._

class RegExpTest extends FlatSpec {
  val char0 = CharExp('0')
  val char1 = CharExp('1')
  val char2 = CharExp('2')
  val empty = EmptyExp
  val eps = EpsExp
  val concat = ConcatExp(char0, char1)
  val alt = AltExp(char0, char1)
  val star = StarExp(concat)

  "CharExp" should "toNFA" in
  {
    val c = char0.toNFA()

    assert(c.states == Set(0,1))
    assert(c.alpha == Set('0'))
    assert(c.q0 == 0)
    assert(c.finalStates == Set(1))
    assert(c.eclosure(Set(0)) == Set(0))
    assert(c.trans(0,List('0')) == Set(1))
    assert(c.accept(List('0')))
  }

  "EmptyExp" should "toNFA" in
  {
    val em = empty.toNFA()

    assert(em.states == Set(0))
    assert(em.alpha == Set())
    assert(em.q0 == 0)
    assert(em.finalStates == Set())
    assert(em.eclosure(Set(0)) == Set(0))
    assert(!em.accept(List()))
  }

  "EpsExp" should "toNFA" in
  {
    val e = eps.toNFA()

    assert(e.states == Set(0))
    assert(e.alpha == Set())
    assert(e.q0 == 0)
    assert(e.finalStates == Set(0))
    assert(e.eclosure(Set(0)) == Set(0))
    assert(e.accept(List()))
  }

  "ConcatExp" should "toNFA" in
  {
    val con = concat.toNFA()

    assert(con.states == Set(0,1,2,3))
    assert(con.alpha == Set('0','1'))
    assert(con.q0 == 0)
    assert(con.finalStates == Set(3))
    assert(con.eclosure(Set(0)) == Set(0))
    assert(con.eclosure(Set(1)) == Set(1,2))
    assert(con.accept(List('0','1')))
    assert(!con.accept(List('0')))
    assert(!con.accept(List('1')))
  }

  "AltExp" should "toNFA" in
  {
    val a = alt.toNFA()

    assert(a.states == Set(0,1,2,3,4))
    assert(a.alpha == Set('0','1'))
    assert(a.q0 == 0)
    assert(a.finalStates == Set(2,4))
    assert(a.eclosure(Set(0)) == Set(0,1,3))
    assert(a.eclosure(Set(1)) == Set(1))
    assert(a.accept(List('0')))
    assert(a.accept(List('1')))
    assert(!a.accept(List('0','1')))
  }

  "StarExp" should "toNFA" in
  {
    val s = star.toNFA()

    assert(s.states == Set(0,1,2,3,4))
    assert(s.alpha == Set('0','1'))
    assert(s.q0 == 0)
    assert(s.finalStates == Set(0,4))
    assert(s.eclosure(Set(0)) == Set(0,1))
    assert(s.accept(List('0','1')))
    assert(!s.accept(List('0','1','1')))
    assert(s.accept(List()))
    assert(s.accept(List('0','1','0','1','0','1')))
    assert(!s.accept(List('0','1','0','1','0','1','0','1','0')))
  }


  "RegExp" should "toNFA" in
  {
    //ε|(0)*1|20(1)*
    val r1 =
    AltExp(                             //+1
      EpsExp, AltExp(                   //+1 +1
        ConcatExp(
          StarExp(char0), char1         //+1(2) 2
        ), ConcatExp(
          char2, ConcatExp(             //2
            char0, StarExp(char1)       //2 +1(2)
          )
        )
      )
    )

    //(01|0)*
    val r2 =
    StarExp(                            //+1
      AltExp(                           //+1
        ConcatExp(char0, char1), char0  //(2 2) 2
      )
    )

    //(0|1)*012
    val r3 =
    ConcatExp(
      StarExp(                          //+1
        AltExp(char0, char1)            //+1(2 2)
      ), ConcatExp(
        char0, ConcatExp(               //2
          char1, char2                  //2 2
        )
      )
    )

    val nfa1 = r1.toNFA()
    val nfa2 = r2.toNFA()
    val nfa3 = r3.toNFA()

    assert(nfa1.states == Range(0, 15).toSet)
    assert(nfa1.alpha == Set('0','1','2'))
    assert(nfa1.q0 == 0)
    assert(nfa1.finalStates == Set(1,7,12,14))
    assert(nfa1.eclosure(Set(0)) == Set(0,1,2,3,4,6,8))
    assert(nfa1.accept(List()))
    assert(nfa1.accept(List('1')))
    assert(nfa1.accept(List('0','0','1')))
    assert(!nfa1.accept(List('1','0')))
    assert(nfa1.accept(List('0','0','0','0','0','1')))
    assert(nfa1.accept(List('2','0','1')))
    assert(nfa1.accept(List('2','0')))
    assert(!nfa1.accept(List('2','1','0')))
    assert(!nfa1.accept(List('2','0','1','0')))
    assert(nfa1.accept(List('2','0','1','1','1','1')))

    assert(nfa2.states == Range(0, 8).toSet)
    assert(nfa2.alpha == Set('0','1'))
    assert(nfa2.q0 == 0)
    assert(nfa2.finalStates == Set(0,5,7))
    assert(nfa2.eclosure(Set(0)) == Set(0,1,2,6))
    assert(nfa2.accept(List()))
    assert(nfa2.accept(List('0')))
    assert(nfa2.accept(List('0','1')))
    assert(nfa2.accept(List('0','0','0','0','0','1')))
    assert(nfa2.accept(List('0','0','1','0','0','0','0')))
    assert(!nfa2.accept(List('1','0','0','0')))
    assert(!nfa2.accept(List('0','1','0','0','0','1','1','0')))

    assert(nfa3.states == Range(0,12).toSet)
    assert(nfa3.alpha == Set('0','1','2'))
    assert(nfa3.q0 == 0)
    assert(nfa3.finalStates == Set(11))
    assert(nfa3.eclosure(Set(0)) == Set(0,1,2,4,6))
    assert(nfa3.accept(List('0','1','2')))
    assert(nfa3.accept(List('0','0','0','0','1','2')))
    assert(!nfa3.accept(List('0','1')))
    assert(!nfa3.accept(List('1','2')))
    assert(!nfa3.accept(List('0','0','0','1','2','0')))
    assert(!nfa3.accept(List('0','1','1','2')))
  }
}