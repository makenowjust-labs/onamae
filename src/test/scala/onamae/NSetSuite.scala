package onamae

class NSetSuite extends munit.FunSuite:
  test("NSet: example"):
    val set = NSet.atoms

    assert(set.contains(Atom(0)))
    assert(set.contains(Atom(100)))

    assertEquals(set.toSet, Set(Atom(0)))

  test("NSet.product"):
    val set = NSet.product(NSet.atoms, NSet.atoms)
    assertEquals(set.toSet, Set((Atom(0), Atom(0)), (Atom(0), Atom(1)), (Atom(1), Atom(0))))

  test("NSet.sepProduct"):
    val set = NSet.sepProduct(NSet.atoms, NSet.atoms)
    assertEquals(set.toSet, Set((Atom(0), Atom(1)), (Atom(1), Atom(0))))

  test("NSet.leftProduct"):
    val set = NSet.leftProduct(NSet.product(NSet.atoms, NSet.atoms), NSet.atoms)
    assertEquals(
      set.toSet,
      Set(
        ((Atom(0), Atom(0)), Atom(0)),
        ((Atom(0), Atom(1)), Atom(0)),
        ((Atom(0), Atom(1)), Atom(1)),
        ((Atom(1), Atom(0)), Atom(0)),
        ((Atom(1), Atom(0)), Atom(1))
      )
    )

  test("NSet.rightProduct"):
    val set = NSet.rightProduct(NSet.atoms, NSet.product(NSet.atoms, NSet.atoms))
    assertEquals(
      set.toSet,
      Set(
        (Atom(0), (Atom(0), Atom(0))),
        (Atom(0), (Atom(0), Atom(1))),
        (Atom(1), (Atom(0), Atom(1))),
        (Atom(0), (Atom(1), Atom(0))),
        (Atom(1), (Atom(1), Atom(0)))
      )
    )

  test("NSet#map"):
    val set = NSet.product(NSet.atoms, NSet(1, 2)).map((a, i) => (a, i + 1))
    assertEquals(set.toSet, Set((Atom(0), 2), (Atom(0), 3)))

  test("NSet#filter"):
    val set = NSet.product(NSet.atoms, NSet.atoms).filter(_ != _)
    assertEquals(set, NSet.sepProduct(NSet.atoms, NSet.atoms))

  test("NSet#quotient"):
    import NSet.{EquivalentClass => EC}
    val set = NSet.product(NSet.atoms, NSet(1, 2, 3))
    val (quot, ks) = set.quotient:
      case (a, b) if a == b                    => true
      case ((_, 1), (_, 2)) | ((_, 2), (_, 1)) => true
      case _                                   => false
    assertEquals(
      quot,
      NMap(
        (Atom(0), 1) -> EC(0, Support.empty),
        (Atom(0), 2) -> EC(0, Support.empty),
        (Atom(0), 3) -> EC(1, Support(Atom(0)))
      )
    )
    assertEquals(ks.toSet, Set(EC(0, Support.empty), EC(1, Support(Atom(0)))))
