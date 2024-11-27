package onamae
package automaton

class LStarSuite extends munit.FunSuite:
  test("LStar.learn: ww(1)"):
    val target = NDFA.ww(1)
    val teacher = Teacher.fromDFA(target)
    val learned = LStar.learn(teacher)

    assertEquals(NDFA.findSepWord(target, learned), None)
    assertEquals(learned.stateSet.size, 4)

  test("LStar.learn: fifo(1)"):
    val target = NDFA.fifo(1)
    val teacher = Teacher.fromDFA(target)
    val learned = LStar.learn(teacher)

    assertEquals(NDFA.findSepWord(target, learned), None)
    assertEquals(learned.stateSet.size, 3)

  test("LStar.learn: fifo(2)"):
    val target = NDFA.fifo(2)
    val teacher = Teacher.fromDFA(target)
    val learned = LStar.learn(teacher)

    assertEquals(NDFA.findSepWord(target, learned), None)
    assertEquals(learned.stateSet.size, 6)
