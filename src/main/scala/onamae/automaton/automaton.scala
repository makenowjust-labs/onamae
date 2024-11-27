package onamae
package automaton

import scala.annotation.tailrec
import scala.collection.mutable

/** NDFA is a nominal DFA. */
final case class NDFA[Q, A](
    stateSet: NSet[Q],
    alphabet: NSet[A],
    initialState: Q,
    acceptStateSet: NSet[Q],
    transitionFunction: NMap[(Q, A), Q]
):

  /** Computes the run of `this` against `word` and checks whether the last state is accepting or not. */
  def run(word: Seq[A])(using Nominal[Q], Nominal[A]): Boolean =
    val q = word.foldLeft(initialState)((q, a) => transitionFunction((q, a)))
    acceptStateSet.contains(q)

/** NDFA utilities and examples. */
object NDFA:

  /** Returns the minimal DFA of `dfa` using Moore's minimization method.
    *
    * This is described in §3.1 of [Venhoek, et al. (2022)], "Fast Computations on Ordered Nominal Sets".
    *
    * [Venhoek, et al. (2022)]: https://www.sciencedirect.com/science/article/abs/pii/S0304397522005291
    */
  def minimize[Q, A](dfa: NDFA[Q, A])(using Nominal[Q], Nominal[A]): NDFA[NSet.EquivalentClass, A] =
    import dfa.{stateSet, alphabet, initialState, acceptStateSet, transitionFunction}

    var (quot, ks) =
      stateSet.quotient((q1, q2) => q1 == q2 || acceptStateSet.contains(q1) == acceptStateSet.contains(q2))
    var done = false
    while !done do
      val (nextQuot, nextKs) = stateSet.quotient: (q1, q2) =>
        quot(q1) == quot(q2) && NSet
          .productIterator(NSet((q1, q2)), alphabet)
          .forall:
            case ((q1, q2), a) => quot(transitionFunction((q1, a))) == quot(transitionFunction((q2, a)))
      done = quot == nextQuot
      quot = nextQuot
      ks = nextKs

    NDFA(
      ks,
      alphabet,
      quot(initialState),
      acceptStateSet.map(quot(_)),
      transitionFunction.map { case ((q1, a), q2) => (quot(q1), a) -> quot(q2) }
    )

  /** Finds a separating word between `dfa1` and `dfa2` using the BFS (Breadth-First Search).
    *
    * A separating word is a word that is accepted by one DFA, but is not accepted by another.
    */
  def findSepWord[Q1, Q2, A](dfa1: NDFA[Q1, A], dfa2: NDFA[Q2, A])(using
      Nominal[Q1],
      Nominal[Q2],
      Nominal[A]
  ): Option[Seq[A]] =
    require(dfa1.alphabet == dfa2.alphabet)
    val alphabet = dfa1.alphabet

    val queue = mutable.Queue.empty[(Q1, Q2, List[A])]
    var visited = NSet.empty[(Q1, Q2)]

    queue.enqueue((dfa1.initialState, dfa2.initialState, Nil))
    visited += (dfa1.initialState, dfa2.initialState)

    while queue.nonEmpty do
      val (q1, q2, word) = queue.dequeue
      if dfa1.acceptStateSet.contains(q1) != dfa2.acceptStateSet.contains(q2) then return Some(word.reverse)
      val nextStatePairSet = NSet.map2(NSet((q1, q2, word)), alphabet):
        case ((q1, q2, word), a) => (dfa1.transitionFunction((q1, a)), dfa2.transitionFunction((q2, a)), a :: word)
      for (p1, p2, nextWord) <- nextStatePairSet.iterator; if !visited.contains((p1, p2)) do
        queue.enqueue((p1, p2, nextWord))
        visited += (p1, p2)

    None

  // ww (double word) example:

  /** WWState is a state for nominal DFAs accepting ww (double word) language. */
  enum WWState[+A] derives Nominal:
    case Accept, Reject
    case Store(as: List[A])
    case Check(as: List[A])

  /** Constructs the ww (double word) example.
    *
    * The parameter `n` specifies the size of the repeated word.
    */
  def ww(n: Int): NDFA[WWState[Atom], Atom] =
    import WWState.{Store, Check, Accept, Reject}
    type Q = WWState[Atom]
    type A = Atom

    def words(n: Int, m: Int): NSet[List[A]] =
      NSet.from((n to m).iterator.flatMap(NSet.atomsList(_).iterator))
    val stateSet = NSet[Q](Accept, Reject) union words(0, n - 1).map(Store(_)) union words(1, n).map(Check(_))

    val initialState: Q = Store(Nil)
    val acceptStateSet: NSet[Q] = NSet(Accept)

    def transition(q: Q, a: A): Q = (q, a) match
      case (Accept | Reject, _)               => Reject
      case (Store(as), a) if as.size + 1 == n => Check((a :: as).reverse)
      case (Store(as), a)                     => Store(a :: as)
      case (Check(a +: as), b) if a == b =>
        if as.isEmpty then Accept else Check(as)
      case (Check(_), _) => Reject

    val transitionKeysIter = NSet.productIterator(stateSet, NSet.atoms)
    val transitionFunction = NMap.tabulate(transitionKeysIter)(transition(_, _))

    NDFA(stateSet, NSet.atoms, initialState, acceptStateSet, transitionFunction)

  // FIFO (First-In First-Out) queue example:

  /** FIFOQueue is a functional FIFO (First-In First-Out) queue implementation. */
  final case class FIFOQueue[A](as1: List[A], as2: List[A]) derives Nominal:

    /** Returns the size of `this`. */
    def size: Int = as1.size + as2.size

    /** Pushes `a` into `this` and returns a new queue. */
    def push(a: A): FIFOQueue[A] = FIFOQueue(a +: as1, as2)

    /** Pops a value from `this` if possible. */
    @tailrec
    def pop: Option[(A, FIFOQueue[A])] = (as1, as2) match
      case (Nil, Nil)      => None
      case (as1, Nil)      => FIFOQueue(Nil, as1.reverse).pop
      case (as1, a :: as2) => Some((a, FIFOQueue(as1, as2)))

  /** FIFOQueue utilities. */
  object FIFOQueue:

    /** Returns the empty queue. */
    def empty[A]: FIFOQueue[A] = FIFOQueue(Nil, Nil)

  /** FIFOAlphabet is a character of the FIFO queue example. */
  enum FIFOAlphabet derives Nominal:
    case Put(atom: Atom)
    case Get(atom: Atom)

  /** Constructs the FIFO queue example DFA.
    *
    * The parameter `n` is a bound of the size of queue.
    */
  def fifo(n: Int): NDFA[Option[FIFOQueue[Atom]], FIFOAlphabet] =
    require(n >= 0)

    import FIFOAlphabet.{Put, Get}
    type Q = Option[FIFOQueue[Atom]]
    type A = FIFOAlphabet

    val alphabet = NSet.atoms.map(Put(_)) union NSet.atoms.map(Get(_))

    def possibleFIFOQueue(i: Int): Iterator[FIFOQueue[Atom]] =
      (0 to i).iterator.flatMap: j =>
        NSet.productIterator(NSet.atomsList(j), NSet.atomsList(i - j)).map(FIFOQueue(_, _))
    val allPossibleFIFOQueue = NSet.from((0 to n).iterator.flatMap(possibleFIFOQueue))

    val stateSet = NSet(Option.empty) union allPossibleFIFOQueue.map(Option(_))

    val initialState = Option(FIFOQueue.empty[Atom])
    val acceptStateSet = stateSet.filter(_.isDefined)

    def transition(q: Q, a: A): Q = (q, a) match
      case (None, _)                                => None
      case (Some(queue), Put(_)) if queue.size >= n => None
      case (Some(queue), Put(atom))                 => Some(queue.push(atom))
      case (Some(queue), Get(atom1)) =>
        queue.pop match
          case None                               => None
          case Some((atom2, _)) if atom1 != atom2 => None
          case Some((_, queue))                   => Some(queue)

    val transitionKeysIter = NSet.productIterator(stateSet, alphabet)
    val transitionFunction = NMap.tabulate(transitionKeysIter)(transition(_, _))

    NDFA(stateSet, alphabet, initialState, acceptStateSet, transitionFunction)
