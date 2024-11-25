package onamae

class SupportSuite extends munit.FunSuite:
  test("Support.default"):
    assertEquals(Support.default(0), Support.empty)
    assertEquals(Support.default(3), Support(Atom(0), Atom(1), Atom(2)))

  test("Support#subsetOf"):
    assert(Support.default(2).subsetOf(Support.default(5)))

  test("Support#toSeq"):
    // The result should be sorted.
    assertEquals(Support.default(2).toSeq, Seq(Atom(0), Atom(1)))
