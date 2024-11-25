package onamae

class NMapSuite extends munit.FunSuite:
  test("NMap (example)"):
    val set = NSet.sepProduct(NSet.atoms, NSet.atoms)
    val map = NMap.fromSet(set): (l, r) =>
      if l < r then Left(l) else Right(r)

    assertEquals(map.get((Atom(0), Atom(1))), Some(Left(Atom(0))))
    assertEquals(map.get((Atom(1), Atom(0))), Some(Right(Atom(0))))
    assertEquals(map.get((Atom(0), Atom(0))), None)

    assertEquals(map.get((Atom(10), Atom(30))), Some(Left(Atom(10))))
