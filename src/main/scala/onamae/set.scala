package onamae

import scala.annotation.tailrec

/** NSet is an equivariant set.
  *
  * On the implementation, this holds a set of orbits of values. Therefore, this set can seemingly contain infinite
  * values with atoms.
  *
  * We can use this much like a `scala.collection.Set`. However, some methods need a `Nominal[A]` instance to work correctly.
  *
  * **Example**:
  *
  * ```scala
  * // `NSet.atoms` is a set of all atoms.
  * val set = NSet.atoms
  *
  * set.contains(Atom(0))   // => true
  * set.contains(Atom(100)) // => true
  *
  * // `NSet#toSet` returns a set of default values of orbits.
  * set.toSet // => Set(Atom(0))
  * ```
  */
final class NSet[A] private[onamae] (private val orbitSet: Set[Nominal[A]#Orbit]):
  // Note that the type of `orbitSet` is `Set[Nominal[A]#Orbit]` because it is hard to decide the right `Orbit` type for `A` in Scala.
  // Thus, we assume the given `Nominal[A]` instances must be the same across multiple method calls and cast `orbitSet` as needed.

  /** Returns the number of orbits contained in `this`.
    *
    * Note that this value is different from the number of values with which `NSet#contains` returns `true`. `NSet` is a nominal set, so
    * such a number may be infinite.
    */
  def size: Int = orbitSet.size

  /** Checks whether `this` is empty or not. */
  def isEmpty: Boolean = orbitSet.isEmpty

  /** Like `NSet#isEmpty`, but it is the opposite. */
  def nonEmpty: Boolean = orbitSet.nonEmpty

  /** Checks whether `this` contains `value` or not. */
  def contains(value: A)(using A: Nominal[A]): Boolean = orbitSet.contains(A.orbitOf(value))

  /** Returns a new equivariant set of `this` with `value`. */
  infix def +(value: A)(using A: Nominal[A]): NSet[A] = new NSet(orbitSet + A.orbitOf(value))

  /** Returns the union set of two equivariant sets. */
  infix def union(that: NSet[A]): NSet[A] = new NSet(orbitSet union that.orbitSet)

  /** Returns the difference set of two equivariant sets. */
  infix def diff(that: NSet[A]): NSet[A] = new NSet(orbitSet diff that.orbitSet)

  /** Returns the intersection set of two equivariant sets. */
  infix def intersect(that: NSet[A]): NSet[A] = new NSet(orbitSet intersect that.orbitSet)

  /** Returns a new equivariant set with values of `this` converted by `f`.
    *
    * Note that `f` should be an equivariant map, i.e., `f` should convert values of one orbit to values of the same orbit.
    */
  def map[B](f: A => B)(using A: Nominal[A], B: Nominal[B]): NSet[B] =
    new NSet(orbitSet.map(orbit => B.orbitOf(f(A.defaultElementOf(orbit.asInstanceOf[A.Orbit])))))

  /** Returns a new equivariant set with values of `this` filtered by `f`.
    *
    * Note that `f` should be an equivariant map.
    */
  def filter(f: A => Boolean)(using A: Nominal[A]): NSet[A] =
    new NSet(orbitSet.filter(orbit => f(A.defaultElementOf(orbit.asInstanceOf[A.Orbit]))))

  /** Returns two equivariant sets partitioned by `f`.
    *
    * Note that `f` should be an equivariant map.
    */
  def partition(f: A => Boolean)(using A: Nominal[A]): (NSet[A], NSet[A]) =
    val (orbitSet1, orbitSet2) = orbitSet.partition(orbit => f(A.defaultElementOf(orbit.asInstanceOf[A.Orbit])))
    (new NSet(orbitSet1), new NSet(orbitSet2))

  /** Checks whether every value of `this` satisfies `f`.
    *
    * Note that `f` should be an equivariant map.
    */
  def forall(f: A => Boolean)(using A: Nominal[A]): Boolean =
    orbitSet.forall(orbit => f(A.defaultElementOf(orbit.asInstanceOf[A.Orbit])))

  /** Checks whether some values of `this` satisfy `f`.
    *
    * Note that `f` should be an equivariant map.
    */
  def exists(f: A => Boolean)(using A: Nominal[A]): Boolean =
    orbitSet.exists(orbit => f(A.defaultElementOf(orbit.asInstanceOf[A.Orbit])))

  /** Returns the result of applying `f` together with `initial` sequentially to the value of `this`.
    *
    * Note that the ordering of applying `f` is unstable because the internal container is `scala.collection.Set`.
    */
  def foldLeft[B](initial: B)(f: (B, A) => B)(using A: Nominal[A]): B =
    orbitSet.foldLeft(initial)((b, orbit) => f(b, A.defaultElementOf(orbit.asInstanceOf[A.Orbit])))

  /** Computes the quotient of `this` by an equivalence relation `f`.
    * The result is a pair of a mapping from values to their equivalent classes and a set of equivalent classes.
    *
    * Note that `f` should be an equivariant map.
    */
  def quotient(f: (A, A) => Boolean)(using A: Nominal[A]): (NMap[A, NSet.EquivalentClass], NSet[NSet.EquivalentClass]) =
    import NSet.{EquivalentClass => EC}
    def loop(n: Int, as: Seq[A], quot: NMap[A, EC], ks: NSet[EC]): (NMap[A, EC], NSet[EC]) =
      as match
        case Seq()                       => (quot, ks)
        case a +: as if quot.contains(a) => loop(n, as, quot, ks)
        case a +: as                     =>
          // `eqSet` is a set of equivalent values to `a`.
          val eqSet = NSet(a) union NSet(as*).filter(b => NSet.productIterator(NSet(a), NSet(b)).exists(f(_, _)))
          // To compute the least support correctly, we need to `eqPairSet`.
          // Note that `filter(f(_, _))` is necessary here because a pair of two `eqSet`s may not be equivalent.
          val eqPairSet = NSet.productIterator(eqSet, eqSet).filter(f(_, _))
          val supportPairSet = eqPairSet.map((a, b) => a -> (A.support(a) intersect A.support(b)))
          val newQuot = supportPairSet.foldLeft(NMap.empty[A, EC]):
            case (newQuot, (a, support1)) =>
              newQuot.get(a) match
                case Some(EC(_, support2)) => newQuot.updated(a, EC(n, support1 intersect support2))
                case None                  => newQuot.updated(a, EC(n, support1))
          loop(n + 1, as, quot ++ newQuot, ks union newQuot.keySet.map(newQuot(_)))
    loop(0, this.toSeq, NMap.empty, NSet.empty)

  /** Returns an iterator to iterate default values of orbits in `this`. */
  def iterator(using A: Nominal[A]): Iterator[A] =
    orbitSet.iterator.map(orbit => A.defaultElementOf(orbit.asInstanceOf[A.Orbit]))

  /** Returns the set of default values of orbits in `this`. */
  def toSet(using A: Nominal[A]): Set[A] =
    iterator.toSet

  /** Like `NSet#toSet`, but it returns `Seq` instead. */
  def toSeq(using A: Nominal[A]): Seq[A] = iterator.toSeq

  override def equals(that: Any): Boolean = that match
    case that: NSet[_] => orbitSet == that.orbitSet
    case _             => false

  override def hashCode(): Int = orbitSet.hashCode()

  override def toString(): String = s"NSet { val orbitSet = $orbitSet }"

/** NSet utilities. */
object NSet:

  /** EquivalentClass is an equivalent class for `NSet#quotient` results. */
  final case class EquivalentClass(index: Int, support: Support) derives Nominal

  // `NSet` is nominal and its instance is trivial because it is equivariant.
  given nominalInstance[A]: Nominal[NSet[A]] = Nominal.derivedTrivially

  /** Returns the empty equivariant set. */
  def empty[A]: NSet[A] = new NSet(Set.empty)

  /** Constructs an equivariant set with the given `values`. */
  def apply[A](values: A*)(using A: Nominal[A]): NSet[A] = NSet.from(values)

  /** Constructs an equivariant set with values of `iter`. */
  def from[A](iter: IterableOnce[A])(using A: Nominal[A]): NSet[A] = new NSet(iter.iterator.map(A.orbitOf).toSet)

  /** An equivariant set contains every atoms. */
  val atoms: NSet[Atom] = NSet(Atom(0))

  /** Returns a set of sequence having `n` atoms. */
  def atomsList(n: Int): NSet[List[Atom]] =
    @tailrec
    def loop(i: Int, atomsList: NSet[List[Atom]]): NSet[List[Atom]] =
      if i == n then atomsList
      else loop(i + 1, NSet.map2(atoms, atomsList)(_ :: _))
    loop(0, NSet(Nil))

  /** Returns the union of all sets of `set`. */
  def union[A](set: NSet[NSet[A]]): NSet[A] = set.foldLeft(NSet.empty)(_ union _)

  /** Iterates each product of two equivariant sets over the given key generator `g`. */
  private def productWith[A: Nominal, B: Nominal](setA: NSet[A], setB: NSet[B])(g: KeyGen): Iterator[(A, B)] =
    val A = summon[Nominal[A]]
    val B = summon[Nominal[B]]
    val AxB = summon[Nominal[(A, B)]]
    for
      orbitA <- setA.orbitSet.asInstanceOf[Set[A.Orbit]].iterator
      orbitB <- setB.orbitSet.asInstanceOf[Set[B.Orbit]].iterator
      keys <- g(A.dim(orbitA), B.dim(orbitB)).iterator
    yield
      val orbitAxB = ProductOrbit(orbitA, orbitB, keys)
      AxB.defaultElementOf(orbitAxB.asInstanceOf[AxB.Orbit])

  /** Like `NSet.product`, but it returns an iterator. */
  def productIterator[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): Iterator[(A, B)] =
    productWith(setA, setB)(KeyGen.product(_, _))

  /** Like `NSet.sepProduct`, but it returns an iterator. */
  def sepProductIterator[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): Iterator[(A, B)] =
    productWith(setA, setB)(KeyGen.sepProduct(_, _))

  /** Like `NSet.leftProduct`, but it returns an iterator. */
  def leftProductIterator[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): Iterator[(A, B)] =
    productWith(setA, setB)(KeyGen.leftProduct(_, _))

  /** Like `NSet.rightProduct`, but it returns an iterator. */
  def rightProductIterator[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): Iterator[(A, B)] =
    productWith(setA, setB)(KeyGen.rightProduct(_, _))

  /** Returns the Cartesian product of two equivariant sets.
    *
    * It constructs a new equivariant set of the Cartesian product, however, sometimes we don't need a whole set and
    * just need to iterate each product instead. In such cases, we can use `NSet#productIterator` that returns `Iterator[(A, B)]`.
    * This is critical when performance is important.
    *
    * **Example**:
    *
    * ```scala
    * val set = NSet.product(NSet.atoms, NSet.atoms)
    * set.toSet // => Set((Atom(0), Atom(0)), (Atom(0), Atom(1)), (Atom(1), Atom(0)))
    * ```
    */
  def product[A, B](setA: NSet[A], setB: NSet[B])(using Nominal[A], Nominal[B]): NSet[(A, B)] =
    NSet.from(productIterator(setA, setB))

  /** Like `NSet.product`, but the result equivariant set does not contain a pair whose supports have an intersection.
    *
    * **Example**:
    *
    * ```scala
    * val set = NSet.sepProduct(NSet.atoms, NSet.atoms)
    * set.toSet // => Set((Atom(0), Atom(1)), (Atom(1), Atom(0)))
    * ```
    */
  def sepProduct[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): NSet[(A, B)] =
    NSet.from(sepProductIterator(setA, setB))

  /** Like `NSet.product`, but the result is left-product.
    * That is, the right value are supported by the support of the left value.
    *
    * **Example**
    *
    * ```scala
    * val set = NSet.leftProduct(NSet.product(NSet.atoms, NSet.atoms), NSet.atoms)
    * set.toSet
    * // => Set(
    * //      ((Atom(0), Atom(0)), Atom(0)),
    * //      ((Atom(0), Atom(1)), Atom(0)),
    * //      ((Atom(0), Atom(1)), Atom(1)),
    * //      ((Atom(1), Atom(0)), Atom(0)),
    * //      ((Atom(1), Atom(0)), Atom(1))
    * //    )
    * ```
    */
  def leftProduct[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): NSet[(A, B)] =
    NSet.from(leftProductIterator(setA, setB))

  /** Like `NSet.product`, but the result is right-product.
    * That is, the left value are supported by the suppor of the right value.
    *
    * **Example**:
    *
    * ```scala
    * val set = NSet.rightProduct(NSet.atoms, NSet.product(NSet.atoms, NSet.atoms))
    * set.toSet
    * // => Set(
    * //      (Atom(0), (Atom(0), Atom(0))),
    * //      (Atom(0), (Atom(0), Atom(1))),
    * //      (Atom(1), (Atom(0), Atom(1))),
    * //      (Atom(0), (Atom(1), Atom(0))),
    * //      (Atom(1), (Atom(1), Atom(0)))
    * //    )
    * ```
    */
  def rightProduct[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): NSet[(A, B)] =
    NSet.from(rightProductIterator(setA, setB))

  /** Returns a new equivariant set with values of two sets converted by `f`.
    *
    * Note that `f` should be an equivariant map.
    */
  def map2[A, B, C](setA: NSet[A], setB: NSet[B])(f: (A, B) => C)(using Nominal[A], Nominal[B], Nominal[C]): NSet[C] =
    NSet.from(productIterator(setA, setB).map(f(_, _)))
