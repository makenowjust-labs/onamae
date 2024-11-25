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
  def nonEmpty: Boolean = orbitSet.isEmpty

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

  /** Computes the quotient of `this` by an equational relation `f`.
    * The result is a pair of a mapping from values to their equivalent classes and a set of equivalent classes.
    *
    * Note that `f` should be an equivariant map.
    */
  def quotient(f: (A, A) => Boolean)(using A: Nominal[A]): (NMap[A, NSet.EquivalentClass], NSet[NSet.EquivalentClass]) =
    import NSet.{EquivalentClass => EC}
    def loop(n: Int, set: NSet[A], quot: NMap[A, EC], ks: NSet[EC]): (NMap[A, EC], NSet[EC]) =
      set.toSet.toSeq match
        case Seq()                       => (quot, ks)
        case a +: as if quot.contains(a) => loop(n, NSet(as*), quot, ks)
        case a +: as                     =>
          // `eqPair` is a set of pairs of `a` and equivalence values to `a`.
          // To compute the least support correctly, we need to keep `eqPair` as a set of pairs, not a set of values.
          val eqPair = NSet.product(NSet(as*), NSet(a)).filter(f(_, _))
          val support = eqPair.foldLeft(A.support(a))((support, aa) => support intersect A.support(aa._1))
          val k = EC(n, support)
          val newQuot = (eqPair.map(_._1) + a).foldLeft(quot)((quot, a) => quot.updated(a, k))
          loop(n + 1, NSet(as*), newQuot, ks + k)
    loop(0, this, NMap.empty, NSet.empty)

  /** Returns the set of default values of orbits in `this`. */
  def toSet(using A: Nominal[A]): Set[A] =
    orbitSet.map(orbit => A.defaultElementOf(orbit.asInstanceOf[A.Orbit]))

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

  /** Returns a equivariant set with the given `values`. */
  def apply[A](values: A*)(using A: Nominal[A]): NSet[A] = new NSet(values.map(A.orbitOf).toSet)

  /** An equivariant set contains every atoms. */
  val atoms: NSet[Atom] = NSet(Atom(0))

  def atomsList(n: Int): NSet[List[Atom]] =
    @tailrec
    def loop(i: Int, atomsList: NSet[List[Atom]]): NSet[List[Atom]] =
      if i == n then atomsList
      else loop(i + 1, NSet.map2(atoms, atomsList)(_ :: _))
    loop(0, NSet(Nil))

  def union[A](seq: Seq[NSet[A]]): NSet[A] = seq.reduceLeft(_ union _)

  /** Computes a product of two equivariant sets under the given product function `f`. */
  private def productWith[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B])(
      f: (A.Orbit, B.Orbit) => Seq[ProductOrbit[A, B]]
  ): NSet[(A, B)] =
    new NSet(
      for
        orbitA <- setA.orbitSet.asInstanceOf[Set[A.Orbit]]
        orbitB <- setB.orbitSet.asInstanceOf[Set[B.Orbit]]
        orbitAxB <- f(orbitA, orbitB)
      yield orbitAxB.asInstanceOf[Nominal[(A, B)]#Orbit]
    )

  /** Returns a set of the product of two equivariant sets. */
  def product[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): NSet[(A, B)] =
    productWith(setA, setB)(Nominal.product[A, B](_, _))

  /** Like `NSet.product`, but the result equivariant set does not contain a pair whose supports have an intersection. */
  def sepProduct[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): NSet[(A, B)] =
    productWith(setA, setB)(Nominal.sepProduct[A, B](_, _))

  /** Like `NSet.product`, but the result is left-product. */
  def leftProduct[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): NSet[(A, B)] =
    productWith(setA, setB)(Nominal.leftProduct[A, B](_, _))

  /** Like `NSet.product`, but the result is right-product. */
  def rightProduct[A, B](setA: NSet[A], setB: NSet[B])(using A: Nominal[A], B: Nominal[B]): NSet[(A, B)] =
    productWith(setA, setB)(Nominal.rightProduct[A, B](_, _))

  /** Returns a new equivariant set with values of two sets converted by `f`.
    *
    * Note that `f` should be an equivariant map.
    */
  def map2[A, B, C](setA: NSet[A], setB: NSet[B])(f: (A, B) => C)(using Nominal[A], Nominal[B], Nominal[C]): NSet[C] =
    product(setA, setB).map(f(_, _))
