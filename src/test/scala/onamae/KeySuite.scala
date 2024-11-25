package onamae

class KeySuite extends munit.FunSuite:
  import Key.{Left, Both, Right}

  test("Key.productKeys"):
    assertEquals(Key.productKeys(1, 1), Seq(Seq(Left, Right), Seq(Both), Seq(Right, Left)))

  test("Key.sepProductKeys"):
    assertEquals(Key.sepProductKeys(1, 1), Seq(Seq(Left, Right), Seq(Right, Left)))

  test("Key.leftProduct"):
    assertEquals(Key.leftProductKeys(2, 1), Seq(Seq(Left, Both), Seq(Both, Left)))
    assertEquals(Key.leftProductKeys(1, 2), Seq.empty)

  test("Key.rightProduct"):
    assertEquals(Key.rightProductKeys(1, 2), Seq(Seq(Both, Right), Seq(Right, Both)))
    assertEquals(Key.rightProductKeys(2, 1), Seq.empty)
