
trait RegExp {
  def toNFA(): NFA[Int,Char]
}

case class CharExp(c: Char) extends RegExp {
  def toNFA(): NFA[Int, Char] = {
    val statesNFA = Set(0, 1)
    val alpha = Set(c)
    val transitionNFA = Map[(Int, Option[Char]), Set[Int]](
      (0, Some(c))->Set(1), (0, None)->Set(0),
      (1, Some(c))->Set(), (1, None)->Set(1)
    )
    val q0 = 0
    val finalStatesNFA = Set(1)

    new NFA(statesNFA, alpha, transitionNFA, q0, finalStatesNFA)
  }
}

// 空集合
case object EmptyExp extends RegExp {
  def toNFA(): NFA[Int, Char] = {
    val statesNFA = Set(0)
    val alpha: Set[Char] = Set()
    val transitionNFA = Map[(Int, Option[Char]), Set[Int]](
      (0, None)->Set(0)
    )
    val q0 = 0
    val finalStatesNFA: Set[Int] = Set()

    new NFA(statesNFA, alpha, transitionNFA, q0, finalStatesNFA)
  }
}

// ε
case object EpsExp extends RegExp {
  def toNFA(): NFA[Int, Char] = {
    val statesNFA = Set(0)
    val alpha: Set[Char] = Set()
    val transitionNFA = Map[(Int, Option[Char]), Set[Int]](
      (0, None)->Set(0)
    )
    val q0 = 0
    val finalStatesNFA = Set(0)

    new NFA(statesNFA, alpha, transitionNFA, q0, finalStatesNFA)
  }
}

// 連接
case class ConcatExp(r1: RegExp, r2: RegExp) extends RegExp {
  def toNFA(): NFA[Int, Char] = {
    val n1: NFA[Int, Char] = r1.toNFA()
    val n2: NFA[Int, Char] = r2.toNFA()
    val n = n1.states.size
    val statesNFA = Range(0, n+n2.states.size).toSet
    val alpha = n1.alpha ++ n2.alpha
    var transitionNFA = Map[(Int, Option[Char]), Set[Int]]()
    val q0 = n1.q0
    val finalStatesNFA = n2.finalStates.map(q => q + n)

    var u = statesNFA.toList

    while (!u.isEmpty) {
      val q = u.head
      u = u.tail
      if (n1.finalStates.contains(q)) {
        for (a <- alpha) {
          if (n1.alpha.contains(a)) transitionNFA = transitionNFA + ((q, Option(a))->n1.trans(q, List(a)))
          else transitionNFA = transitionNFA + ((q, Option(a))->Set())
        }
        transitionNFA = transitionNFA + ((q, None)->(Set(n2.q0+n) ++ n1.trans(q, List())))
      } else if (n1.states.contains(q)) {
        for (a <- alpha) {
          if (n1.alpha.contains(a)) transitionNFA = transitionNFA + ((q, Option(a))->n1.trans(q, List(a)))
          else transitionNFA = transitionNFA + ((q, Option(a))->Set())
        }
        transitionNFA = transitionNFA + ((q, None)->n1.trans(q, List()))
      } else {
        for (a <- alpha) {
          if (n2.alpha.contains(a)) transitionNFA = transitionNFA + ((q, Option(a))->n2.trans(q-n, List(a)).map(q => q + n))
          else transitionNFA = transitionNFA + ((q, Option(a))->Set())
        }
        transitionNFA = transitionNFA + ((q, None)->(n2.trans(q-n, List()).map(q => q + n)))
      }
    }

    new NFA(statesNFA, alpha, transitionNFA, q0, finalStatesNFA)
  }
}

//選択
case class AltExp(r1: RegExp, r2: RegExp) extends RegExp {
  def toNFA(): NFA[Int, Char] = {
    val n1: NFA[Int, Char] = r1.toNFA()
    val n2: NFA[Int, Char] = r2.toNFA()
    val n = n1.states.size+1
    val statesNFA = Range(0, n+n2.states.size).toSet
    val alpha = n1.alpha ++ n2.alpha
    var transitionNFA = Map[(Int, Option[Char]), Set[Int]]()
    val q0 = 0
    val finalStatesNFA = n1.finalStates.map(q => q + 1) ++ n2.finalStates.map(q => q + n)

    var u = statesNFA.toList

    while (!u.isEmpty) {
      val q = u.head
      u = u.tail
      if (q == q0) {
        for (a <- alpha) transitionNFA = transitionNFA + ((q, Option(a))->Set())
        transitionNFA = transitionNFA + ((q, None)->Set(n1.q0 + 1, n2.q0 + n))
      } else if (n1.states.contains(q-1)) {
        for (a <- alpha) {
          if (n1.alpha.contains(a)) transitionNFA = transitionNFA + ((q, Option(a))->n1.trans(q-1, List(a)).map(q => q + 1))
          else transitionNFA = transitionNFA + ((q, Option(a))->Set())
        }
        transitionNFA = transitionNFA + ((q, None)->n1.trans(q-1, List()).map(q => q + 1))
      } else {
        for (a <- alpha) {
          if (n2.alpha.contains(a)) transitionNFA = transitionNFA + ((q, Option(a))->n2.trans(q-n, List(a)).map(q => q + n))
          else transitionNFA = transitionNFA + ((q, Option(a))->Set())
        }
        transitionNFA = transitionNFA + ((q, None)->n2.trans(q-n, List()).map(q => q + n))
      }
    }

    new NFA(statesNFA, alpha, transitionNFA, q0, finalStatesNFA)
  }
}

//繰り返し
case class StarExp(r: RegExp) extends RegExp {
  def toNFA(): NFA[Int, Char] = {
    val n: NFA[Int, Char] = r.toNFA()
    val q0 = 0
    val statesNFA = Range(0, n.states.size+1).toSet
    val alpha = n.alpha
    var transitionNFA = Map[(Int, Option[Char]), Set[Int]]()
    val finalStatesNFA = n.finalStates.map(q => q + 1) + q0

    var u = statesNFA.toList

    while (!u.isEmpty) {
      val q = u.head
      u = u.tail
      if (n.finalStates.contains(q-1)) {
        for (a <- alpha) transitionNFA = transitionNFA + ((q, Option(a))->n.trans(q-1, List(a)).map(q => q + 1))
        transitionNFA = transitionNFA + ((q, None)->(Set(n.q0+1)++n.trans(q-1, List()).map(q => q + 1)))
      } else if (n.states.contains(q-1)) {
        for (a <- alpha) transitionNFA = transitionNFA + ((q, Option(a))->n.trans(q-1, List(a)).map(q => q + 1))
        transitionNFA = transitionNFA + ((q, None)->n.trans(q-1, List()).map(q => q + 1))
      } else {
        for (a <- alpha) transitionNFA = transitionNFA + ((q, Option(a))->Set())
        transitionNFA = transitionNFA + ((q, None)->Set(n.q0 + 1))
      }
    }

    new NFA(statesNFA, alpha, transitionNFA, q0, finalStatesNFA)
  }
}
