package onamae
package automaton

class LStarSuite extends munit.FunSuite:
  test("LStar.learn: fifo(1)"):
    val target = NDFA.fifo(1)
    val teacher = LStar.Teacher.fromDFA(target)
    val learned = LStar.learn(teacher)

    assertEquals(NDFA.findSepWord(target, learned), None)
    assertEquals(learned.stateSet.size, 3)

  test("LStar.learn: fifo(2)".ignore): // This test is too slow (It takes 1~2 seconds).
    val target = NDFA.fifo(2)
    val teacher = LStar.Teacher.fromDFA(target)
    val learned = LStar.learn(teacher)

    assertEquals(NDFA.findSepWord(target, learned), None)
    assertEquals(learned.stateSet.size, 6)
