package onamae

class KeyGenSuite extends munit.FunSuite:
  import Key.{Left, Both, Right}

  test("KeyGen.productKeys"):
    assertEquals(KeyGen.product(1, 1), Seq(Seq(Left, Right), Seq(Both), Seq(Right, Left)))

  test("KeyGen.sepProductKeys"):
    assertEquals(KeyGen.sepProduct(1, 1), Seq(Seq(Left, Right), Seq(Right, Left)))

  test("KeyGen.leftProduct"):
    assertEquals(KeyGen.leftProduct(2, 1), Seq(Seq(Left, Both), Seq(Both, Left)))
    assertEquals(KeyGen.leftProduct(1, 2), Seq.empty)

  test("KeyGen.rightProduct"):
    assertEquals(KeyGen.rightProduct(1, 2), Seq(Seq(Both, Right), Seq(Right, Both)))
    assertEquals(KeyGen.rightProduct(2, 1), Seq.empty)
