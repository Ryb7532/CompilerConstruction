package nonscala

import org.scalatest._
import Base._
import IL._
import Oper._
import AsmExec._

class CodegenTest extends FlatSpec {

  def exec(fenv: FEnv, env: Env, c: Asm.Code): (Env, Mem) = {
    val (env1, mem1, alloc1) = execCode(fenv, env, Map(), 16000, c)
    (env1, mem1)
  }

  "Var" should "正しい値" in
  {
    assert(exec(Map(), Map("x"->1),
      Codegen.genInstr(AssignInstr("y", ValExp(VarVal("x")))))._1("y") == 1)
  }

  "+" should "正しい値" in
  {
    assert(exec(Map(), Map("x"->1),
      Codegen.genInstr(AssignInstr("y", BOpExp(PlusOp, VarVal("x"), IntVal(2)))))._1("y") == 3)
  }

  "-" should "正しい値" in
  {
    assert(exec(Map(), Map("x"->1),
      Codegen.genInstr(AssignInstr("y", BOpExp(MinusOp, VarVal("x"), IntVal(2)))))._1("y") == -1)
  }

  "*" should "正しい値" in
  {
    assert(exec(Map(), Map("x"->2),
      Codegen.genInstr(AssignInstr("y", BOpExp(TimesOp, VarVal("x"), IntVal(2)))))._1("y") == 4)
  }

  "/" should "正しい値" in
  {
    assert(exec(Map(), Map("x"->5),
      Codegen.genInstr(AssignInstr("y", BOpExp(DivideOp, VarVal("x"), IntVal(2)))))._1("y") == 5/2)
  }

  "例: arith (x-y) * z" should "正しい値" in
  {
    val ds = Test.codegenFileDefs("examples/arith.scala")
    val fenv = AsmExec.defs2env(ds)
      val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs(0) ->4, Asm.argRegs(1) -> 2, Asm.argRegs(2) -> 3), Map(), 16000, List(Asm.Callq("test", 3)))
    assert(env2(Asm.retReg) == 6)
  }

  "例: fact" should "正しい値" in
  {
    val ds = Test.codegenFileDefs("examples/fact.scala")
    val fenv = AsmExec.defs2env(ds)
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs(0) ->4), Map(), 16000, List(Asm.Callq("fact", 1)))
    assert(env2(Asm.retReg) == 24)
  }

  def memOf(l: List[Int]): (Mem, Int, Int) = {
    val a0 = 16000
    def loop(l: List[Int]): (Mem, Int, Int) =
      l match {
        case Nil => (Map(), a0, 0)
        case i::l => {
          val (mem, alloc, a) = loop(l)
          val mem1 = mem + (alloc -> i)
          val mem2 = mem1 + (alloc + 8 -> a)
          (mem2, alloc+16, alloc)
        }
      }
    val (mem, alloc, a) = loop(l)
    (mem, a, alloc)
  }


  "例: sort" should "正しい値" in
  {
    val ds = Test.codegenFileDefs("examples/sort.scala")
    val fenv = AsmExec.defs2env(ds)
    val (mem, a0, a1) = memOf(List(3,2,1))
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs(0) ->a0), mem, a1, List(Asm.Callq("sort", 1)))
    assert(mem2list(mem2, env2(Asm.retReg)) == List(1,2,3))
  }

  "例: qsort" should "正しい値" in
  {
    val ds = Test.codegenFileDefs("examples/qsort.scala")
    val fenv = AsmExec.defs2env(ds)
    val (mem, a0, a1) = memOf(List(3,2,1,3,5,3,1,4))
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs(0) ->a0), mem, a1, List(Asm.Callq("qsort", 1)))
    assert(mem2list(mem2, env2(Asm.retReg)) == List(1,1,2,3,3,3,4,5))
  }

  "例: primes" should "正しい値" in
  {
    val ds = Test.codegenFileDefs("examples/primes.scala")
    val fenv = AsmExec.defs2env(ds)
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs(0) ->20), Map(), 16000, List(Asm.Callq("primes", 1)))
    assert(mem2list(mem2, env2(Asm.retReg)) == List(2,3,5,7,11,13,17,19))
  }

  //拡張
  def memOf2(l1: List[Int], l2: List[Int]): (Mem, Int, Int, Int) = {
    val a0 = 16000
    def loop(l: List[Int], mem0: Mem, a0: Int): (Mem, Int, Int) =
      l match {
        case Nil => (mem0, a0, 0)
        case i::l => {
          val (mem, alloc, a) = loop(l, mem0, a0)
          val mem1 = mem + (alloc -> i)
          val mem2 = mem1 + (alloc + 8 -> a)
          (mem2, alloc+16, alloc)
        }
      }
    val (mem1, alloc1, a1) = loop(l1, Map(), a0)
    val (mem2, alloc2, a2) = loop(l2, mem1, alloc1)
    (mem2, a1, a2, alloc2)
  }

  "examples/append" should "正しい値" in
  {
    val ds = Test.codegenFileDefs("examples/append.scala")
    val fenv = AsmExec.defs2env(ds)
    val (mem, a0, a1, alloc) = memOf2(List(1, 2, 3), List(4, 5))
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs(0) ->a0, Asm.argRegs(1)->a1), mem, alloc, List(Asm.Callq("append", 2)))
    assert(mem2list(mem2, env2(Asm.retReg)) == List(1, 2, 3, 4, 5))
  }

  //確認
  "examples/remainder" should "正しい値" in
  {
    val ds = Test.codegenFileDefs("examples/remainder.scala")
    val fenv = AsmExec.defs2env(ds)
    val (env1, mem1, _) =
      execCode(fenv, Map(Asm.argRegs(0) ->2, Asm.argRegs(1) ->2), Map(), 16000, List(Asm.Callq("test", 2)))
    assert(env1(Asm.retReg) == 0)
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs(0) ->5, Asm.argRegs(1) ->3), Map(), 16000, List(Asm.Callq("test", 2)))
    assert(env2(Asm.retReg) == 2)
  }
}
