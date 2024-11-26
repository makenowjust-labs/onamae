package onamae

class NDFASuite extends munit.FunSuite:
  test("NDFA.fifo"):
    import NDFA.FIFOAlphabet.{Put, Get}
    val dfa = NDFA.fifo(2)

    assert(dfa.run(Seq.empty))
    assert(dfa.run(Seq(Put(Atom(0)))))
    assert(dfa.run(Seq(Put(Atom(0)), Get(Atom(0)))))
    assert(dfa.run(Seq(Put(Atom(100)), Get(Atom(100)))))
    assert(dfa.run(Seq(Put(Atom(0)), Get(Atom(0)), Put(Atom(1)), Put(Atom(2)), Get(Atom(1)), Get(Atom(2)))))

    assert(!dfa.run(Seq(Get(Atom(0)))))
    assert(!dfa.run(Seq(Put(Atom(1)), Get(Atom(0)))))

  test("NDFA.minimize"):
    import NDFA.FIFOAlphabet.{Put, Get}
    val dfa = NDFA.minimize(NDFA.fifo(2))

    assert(dfa.run(Seq.empty))
    assert(dfa.run(Seq(Put(Atom(0)))))
    assert(dfa.run(Seq(Put(Atom(0)), Get(Atom(0)))))
    assert(dfa.run(Seq(Put(Atom(100)), Get(Atom(100)))))
    assert(dfa.run(Seq(Put(Atom(0)), Get(Atom(0)), Put(Atom(1)), Put(Atom(2)), Get(Atom(1)), Get(Atom(2)))))

    assert(!dfa.run(Seq(Get(Atom(0)))))
    assert(!dfa.run(Seq(Put(Atom(1)), Get(Atom(0)))))

  test("NDFA.findSepWord"):
    import NDFA.FIFOAlphabet.{Put, Get}

    assertEquals(NDFA.findSepWord(NDFA.fifo(1), NDFA.fifo(2)), Some(Seq(Put(Atom(0)), Put(Atom(1)))))
    assertEquals(NDFA.findSepWord(NDFA.fifo(2), NDFA.minimize(NDFA.fifo(2))), None)
