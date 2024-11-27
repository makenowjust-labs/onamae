package onamae
package automaton

class NDFASuite extends munit.FunSuite:
  test("NDFA.ww"):
    val ww2 = NDFA.ww(2)

    assert(ww2.run(Seq(Atom(0), Atom(0), Atom(0), Atom(0))))
    assert(ww2.run(Seq(Atom(0), Atom(1), Atom(0), Atom(1))))
    assert(ww2.run(Seq(Atom(1), Atom(0), Atom(1), Atom(0))))

    assert(!ww2.run(Seq.empty))
    assert(!ww2.run(Seq(Atom(0))))
    assert(!ww2.run(Seq(Atom(0), Atom(0))))
    assert(!ww2.run(Seq(Atom(0), Atom(0), Atom(0))))
    assert(!ww2.run(Seq(Atom(0), Atom(0), Atom(0), Atom(1))))

  test("NDFA.fifo"):
    import NDFA.FIFOAlphabet.{Put, Get}
    val fifo2 = NDFA.fifo(2)

    assert(fifo2.run(Seq.empty))
    assert(fifo2.run(Seq(Put(Atom(0)))))
    assert(fifo2.run(Seq(Put(Atom(0)), Get(Atom(0)))))
    assert(fifo2.run(Seq(Put(Atom(100)), Get(Atom(100)))))
    assert(fifo2.run(Seq(Put(Atom(0)), Get(Atom(0)), Put(Atom(1)), Put(Atom(2)), Get(Atom(1)), Get(Atom(2)))))

    assert(!fifo2.run(Seq(Get(Atom(0)))))
    assert(!fifo2.run(Seq(Put(Atom(1)), Get(Atom(0)))))

  test("NDFA.minimize: ww"):
    val ww2 = NDFA.ww(2)
    val min = NDFA.minimize(ww2)

    assertEquals(NDFA.findSepWord(ww2, min), None)
    assertEquals(min.stateSet.size, 8)

  test("NDFA.minimize: fifo"):
    val fifo2 = NDFA.fifo(2)
    val min = NDFA.minimize(fifo2)

    assertEquals(NDFA.findSepWord(fifo2, min), None)
    assertEquals(min.stateSet.size, 6)

  test("NDFA.findSepWord: ww"):
    val ww2 = NDFA.ww(2)
    val empty = NDFA(NSet(0), NSet.atoms, 0, NSet.empty, NMap((0, Atom(0)) -> 0))

    assertEquals(NDFA.findSepWord(ww2, empty), Some(List(Atom(0), Atom(1), Atom(0), Atom(1))))

  test("NDFA.findSepWord: fifo"):
    import NDFA.FIFOAlphabet.{Put, Get}
    val fifo2 = NDFA.fifo(2)
    val empty = NDFA(NSet(0), fifo2.alphabet, 0, NSet.empty, NMap.from(fifo2.alphabet.iterator.map(a => (0, a) -> 0)))

    assertEquals(NDFA.findSepWord(fifo2, empty), Some(Nil))
