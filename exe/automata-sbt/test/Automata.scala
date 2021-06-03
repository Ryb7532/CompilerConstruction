import org.scalatest._

class DFATest extends FlatSpec {
  "DFA" should "遷移" in
  {
    val dfa = new DFA(Set(0,1,2),Set('0','1'),
      Map(
        (0,'0')->2,
        (0,'1')->0,
        (1,'0')->0,
        (1,'1')->1,
        (2,'0')->2,
        (2,'1')->1),
      0,Set(1))

    assert(dfa.trans(0,List('0','1')) == 1)
    assert(dfa.accept(List('0','1')))
    assert(!dfa.accept(List('1', '1', '0')))
  }
}

class NFATest extends FlatSpec {
  val trans1 =
    Map(
      (0,None)->Set(1),
      (1,Some('0'))->Set(0),
      (1,None)->Set(0))
  val nfa1 = new NFA(Set(0,1),Set('0'),trans1,0,Set(1))

  val trans2 =
    Map(
      (0,Some('0'))->Set(0),
      (0,None)->Set(1),
      (1,Some('1'))->Set(1),
      (1,None)->Set(2),
      (2,Some('2'))->Set(2))
  val nfa2 = new NFA(Set(0,1,2),Set('0','1','2'),trans2,0,Set(2))

  val trans3 =
    Map(
      (0, None)->Set(2),
      (0, Some('1'))->Set(1),
      (1, Some('0'))->Set(1, 2),
      (1, Some('1'))->Set(2),
      (2, Some('0'))->Set(0))
  val nfa3 = new NFA(Set(0,1,2),Set('0','1','2'),trans3,0,Set(0))

  "NFA" should "ε-closure" in
  {
    assert(nfa1.eclosure(Set(0)) == Set(0,1))
    assert(nfa2.eclosure(Set(0)) == Set(0,1,2))
    assert(nfa3.eclosure(Set(0)) == Set(0,2))
  }

  "NFA" should "tansition" in
  {
    assert(nfa1.trans(0, List('0')) == Set(0,1))
    assert(nfa2.trans(0, List('1')) == Set(1,2))
    assert(nfa3.trans(0, List('0')) == Set(0,2))
    assert(nfa3.trans(2, List('1')) == Set())
    assert(nfa3.trans(0, List('1','0','0')) == Set(0,1,2))
  }

  "NFA" should "accept" in
  {
    assert(nfa1.accept(List('0')))
    assert(nfa2.accept(List('0','1','1')))
    assert(nfa2.accept(List('0','1','1','2')))
    assert(!nfa2.accept(List('0','1','0')))
    assert(nfa3.accept(List('0')))
    assert(nfa3.accept(List('0','1','0','0')))
    assert(!nfa3.accept(List('1','1','1')))
  }

  "NFA" should "サブセット構成" in
  {
    var dfa = nfa2.toDFA()
    assert(dfa.states == Set(Set(0,1,2),Set(1,2),Set(2),Set()))
    assert(dfa.accept(List('0','0','2')))
    assert(!dfa.accept(List('0','0','2','1')))
    assert(dfa.transition(Set(0,1,2),'0')== Set(0,1,2)) 
    assert(dfa.transition(Set(0,1,2),'1')== Set(1,2))
    assert(dfa.transition(Set(0,1,2),'2')== Set(2))
    assert(dfa.transition(Set(1,2),'0')== Set())
    assert(dfa.transition(Set(1,2),'1')== Set(1,2))
    assert(dfa.transition(Set(1,2),'2')== Set(2))
    assert(dfa.transition(Set(2),'0')== Set())
    assert(dfa.transition(Set(2),'1')== Set())
    assert(dfa.transition(Set(2),'2')== Set(2))
    assert(dfa.transition(Set(),'0')== Set())
    assert(dfa.transition(Set(),'1')== Set())
    assert(dfa.transition(Set(),'2')== Set()) 

    dfa = nfa3.toDFA()
    assert(dfa.states == Set(Set(),Set(2),Set(0,2),Set(1),Set(1,2),Set(0,1,2)))
    assert(dfa.accept(List('0','1','0','0')))
    assert(!dfa.accept(List('1','1','1')))
    assert(dfa.transition(Set(),'0') == Set())
    assert(dfa.transition(Set(),'1') == Set())
    assert(dfa.transition(Set(2),'0') == Set(0,2))
    assert(dfa.transition(Set(2),'1') == Set())
    assert(dfa.transition(Set(0,2),'0') == Set(0,2))
    assert(dfa.transition(Set(0,2),'1') == Set(1))
    assert(dfa.transition(Set(1),'0') == Set(1,2))
    assert(dfa.transition(Set(1),'1') == Set(2))
    assert(dfa.transition(Set(1,2),'0') == Set(0,1,2))
    assert(dfa.transition(Set(1,2),'1') == Set(2))
    assert(dfa.transition(Set(0,1,2),'0') == Set(0,1,2))
    assert(dfa.transition(Set(0,1,2),'1') == Set(1,2))
  }
}
