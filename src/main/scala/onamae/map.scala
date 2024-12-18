package onamae

import scala.collection.BitSet
import scala.collection.SortedSet

/** NMap is an equivariant map.
  *
  * We can use this much like a `scala.collection.Map`. However, some methods need a `Nominal[K]` and/or `Nominal[V]`
  * instance(s) to work correctly.
  *
  * **Example**:
  *
  * ```scala
  * val set = NSet.sepProductIterator(NSet.atoms, NSet.atoms)
  * val map = NMap.tabulate(set): (l, r) =>
  *   if l < r then Left(l) else Right(r)
  *
  * map.get((Atom(0), Atom(1)))   // => Some(Left(Atom(0)))
  * map.get((Atom(1), Atom(0)))   // => Some(Right(Atom(0)))
  * map.get((Atom(0), Atom(0)))   // => None
  *
  * map.get((Atom(10), Atom(30))) // => Some(Left(Atom(10)))
  * ```
  */
final class NMap[K, V] private (private val orbitMap: Map[Nominal[K]#Orbit, NMap.Entry[V]]):

  /** Returns the number of key orbits in `this`. */
  def size: Int = orbitMap.size

  /** Checks whether `this` is empty or not. */
  def isEmpty: Boolean = orbitMap.isEmpty

  /** Like `NSet#isEmpty`, but it is the opposite. */
  def nonEmpty: Boolean = orbitMap.nonEmpty

  /** Checks whether `this` contains `key` or not. */
  def contains(key: K)(using K: Nominal[K]): Boolean = orbitMap.contains(K.orbitOf(key))

  /** Returns an equivariant set of keys of `this`. */
  def keySet: NSet[K] = new NSet(orbitMap.keySet)

  /** Finds a value corresponding to `key` and returns the value if it is found.
    * If it is not found, it returns `None` instead.
    */
  def get(key: K)(using K: Nominal[K], V: Nominal[V]): Option[V] =
    orbitMap.get(K.orbitOf(key)).map(_.decode(K.support(key)))

  /** Like `NMap#get`, but it throws an exception if it is not found. */
  def apply(key: K)(using Nominal[K], Nominal[V]): V =
    get(key).getOrElse(throw new NoSuchElementException(s"Key not found: $key"))

  /** Returns a new equivariant map of `this` updated with `key` and `value`. */
  def updated(key: K, value: V)(using K: Nominal[K], V: Nominal[V]): NMap[K, V] =
    new NMap(orbitMap + (K.orbitOf(key) -> NMap.Entry.encode(key, value)))

  /** An infix form of `NMap#updated`. */
  infix def +(kv: (K, V))(using Nominal[K], Nominal[V]) = updated(kv._1, kv._2)

  /** Returns the union of two equivariant map. When a key is conflict, the latter value is preferred. */
  def ++(that: NMap[K, V])(using Nominal[K], Nominal[V]): NMap[K, V] =
    unionWith(that)((_, v) => v)

  /** Returns a new equivariant map of `this` removed `key` and its value. */
  def removed(key: K)(using K: Nominal[K]): NMap[K, V] = new NMap(orbitMap.removed(K.orbitOf(key)))

  /** Returns the union of two equivariant maps. When a key is conflict, values are computed by `f`. */
  def unionWith(that: NMap[K, V])(f: (V, V) => V)(using K: Nominal[K], V: Nominal[V]): NMap[K, V] =
    val newOrbitMap = (orbitMap.keySet union that.orbitMap.keySet).iterator.map: orbitK =>
      val e = (orbitMap.get(orbitK), that.orbitMap.get(orbitK)) match
        case (Some(e), None) => e
        case (None, Some(e)) => e
        case (Some(e1), Some(e2)) =>
          val key = K.defaultElementOf(orbitK.asInstanceOf[K.Orbit])
          val supportK = K.support(key)
          val value1 = e1.decode(supportK)
          val value2 = e2.decode(supportK)
          NMap.Entry.encode(key, f(value1, value2))
        case (None, None) => sys.error("BUG: unreachable")
      orbitK -> e
    new NMap(newOrbitMap.toMap)

  /** Returns the intersection of two equivariant maps. Values are computed by `f`. */
  def intersectWith[V1, V2](that: NMap[K, V1])(
      f: (V, V1) => V2
  )(using K: Nominal[K], V: Nominal[V], V1: Nominal[V1], V2: Nominal[V2]): NMap[K, V2] =
    val newOrbitMap = (orbitMap.keySet intersect that.orbitMap.keySet).iterator.map: orbitK =>
      val v1 = orbitMap(orbitK)
      val v2 = that.orbitMap(orbitK)
      val key = K.defaultElementOf(orbitK.asInstanceOf[K.Orbit])
      val supportK = K.support(key)
      val value1 = v1.decode(supportK)
      val value2 = v2.decode(supportK)
      orbitK -> NMap.Entry.encode(key, f(value1, value2))
    new NMap(newOrbitMap.toMap)

  /** Returns a new equivariant map with key-value pairs of `this` converted by `f`. */
  def map[K1, V1](
      f: (K, V) => (K1, V1)
  )(using K: Nominal[K], V: Nominal[V], K1: Nominal[K1], V1: Nominal[V1]): NMap[K1, V1] =
    val newOrbitMap = orbitMap.iterator.map: (orbitK, e) =>
      val key = K.defaultElementOf(orbitK.asInstanceOf[K.Orbit])
      val value = e.decode(K.support(key))
      f(key, value)
    NMap.from(newOrbitMap)

  /** Returns a new equivariant map with values of `this` converted by `f`. */
  def mapValues[V1](f: V => V1)(using K: Nominal[K], V: Nominal[V], V1: Nominal[V1]): NMap[K, V1] =
    transform((_, value) => f(value))

  /** Like `NMap#mapValues`, but values are computed from pairs of keys and values. */
  def transform[V1](f: (K, V) => V1)(using K: Nominal[K], V: Nominal[V], V1: Nominal[V1]): NMap[K, V1] =
    val newOrbitMap = orbitMap.map: (orbitK, e) =>
      val key = K.defaultElementOf(orbitK.asInstanceOf[K.Orbit])
      val value = e.decode(K.support(key))
      orbitK -> NMap.Entry.encode(key, f(key, value))
    new NMap(newOrbitMap)

  /** Returns an iterator to key-value pairs of `this`. */
  def iterator(using K: Nominal[K], V: Nominal[V]): Iterator[(K, V)] =
    orbitMap.iterator.map: (orbitK, e) =>
      val key = K.defaultElementOf(orbitK.asInstanceOf[K.Orbit])
      val value = e.decode(K.support(key))
      key -> value

  /** Returns the map of default values of orbits in `this`. */
  def toMap(using K: Nominal[K], V: Nominal[V]): Map[K, V] =
    orbitMap.map: (orbitK, e) =>
      val key = K.defaultElementOf(orbitK.asInstanceOf[K.Orbit])
      val value = e.decode(K.support(key))
      key -> value

  override def equals(that: Any): Boolean = that match
    case that: NMap[?, ?] => orbitMap == that.orbitMap

  override def hashCode(): Int = orbitMap.hashCode()

  override def toString(): String = s"NMap { val orbitMap = $orbitMap }"

/** NMap utilities. */
object NMap:
  /** Entry is an internal structure of entries of equivariant maps (i.e., orbits of values).
    *
    * This also holds a filter for supports of keys. It is used to obtain a value from `orbitV`. Because of this
    * implementation, a value should be supported by the support of its key. It is important to keep a map equivariant.
    */
  private final case class Entry[V](orbitV: Nominal[V]#Orbit, supportFilter: BitSet):

    /** Returns a value obtained from `orbitV`. */
    def decode(support: Support)(using V: Nominal[V]): V =
      val atomsV = support.toSortedSet.iterator.zipWithIndex.filter((_, i) => supportFilter.contains(i)).map(_._1)
      val supportV = Support(SortedSet.from(atomsV))
      V.elementOf(orbitV.asInstanceOf[V.Orbit], supportV)

  private object Entry:

    /** Checks a value of `supportV` is supported by `supportK` of a key and returns a filter for `supportK`. */
    def computeSupportFilter(supportK: Support, supportV: Support): BitSet =
      val supportFilter = BitSet.newBuilder
      val ks = supportK.toSeq
      val vs = supportV.toSeq
      var i = 0
      for v <- vs do
        val j = ks.indexOf(v, i)
        if j == -1 then throw new IllegalArgumentException("Not an equivariant map")
        supportFilter.addOne(j)
        i = j + 1
      supportFilter.result()

    /** Encodes a pair of `key` and `value` into an entry of equivariant maps. */
    def encode[K, V](key: K, value: V)(using K: Nominal[K], V: Nominal[V]): Entry[V] =
      val supportK = K.support(key)
      val supportV = V.support(value)
      val supportFilter = computeSupportFilter(supportK, supportV)
      Entry(V.orbitOf(value), supportFilter)

  given nominalInstance[K, V]: Nominal[NMap[K, V]] = Nominal.derivedTrivially

  /** Returns the empty equivariant map. */
  def empty[K, V]: NMap[K, V] = new NMap(Map.empty)

  /** Constructs an equivariant map with key-value pairs of `iter`. */
  def from[K, V](iter: IterableOnce[(K, V)])(using K: Nominal[K], V: Nominal[V]): NMap[K, V] =
    val orbitMap = Map.newBuilder[Nominal[K]#Orbit, Entry[V]]
    orbitMap.sizeHint(iter)
    for (key, value) <- iter.iterator do
      val orbitK = K.orbitOf(key)
      val e = Entry.encode(key, value)
      orbitMap.addOne(orbitK -> e)
    new NMap(orbitMap.result())

  /** Constructs an equivariant map with the given key-value pairs. */
  def apply[K, V](kvs: (K, V)*)(using K: Nominal[K], V: Nominal[V]): NMap[K, V] = from(kvs)

  /** Constructs an equivariant map from a key iterator `iter` and a key-to-value mapping function `f`. */
  def tabulate[K, V](iter: IterableOnce[K])(f: K => V)(using K: Nominal[K], V: Nominal[V]): NMap[K, V] =
    from(iter.iterator.map(key => key -> f(key)))
