package onamae

import scala.annotation.tailrec

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
  def minimize[Q, A](dfa: NDFA[Q, A])(using Q: Nominal[Q], A: Nominal[A]): NDFA[NSet.EquivalentClass, A] =
    import dfa.{stateSet, alphabet, initialState, acceptStateSet, transitionFunction}

    var (quot, ks) =
      stateSet.quotient((q1, q2) => q1 == q2 || acceptStateSet.contains(q1) == acceptStateSet.contains(q2))
    var done = false
    while !done do
      val (nextQuot, nextKs) = stateSet.quotient: (q1, q2) =>
        def transitionEq(q12: (Q, Q), a: A): Boolean =
          val (q1, q2) = q12
          quot(transitionFunction((q1, a))) == quot(transitionFunction((q2, a)))
        quot(q1) == quot(q2) && NSet.product(NSet((q1, q2)), alphabet).forall(transitionEq)
      done = quot == nextQuot
      quot = nextQuot
      ks = nextKs

    val transitions = transitionFunction.toMap.iterator.map:
      case ((q1, a), q2) => (quot(q1), a) -> quot(q2)

    NDFA(
      ks,
      alphabet,
      quot(initialState),
      acceptStateSet.map(quot(_)),
      NMap(transitions.toSeq*)
    )

  // FIFO (First-In First-Out) queue example:

  /** FIFOQueue is a function FIFO (First-In First-Out) queue implementation. */
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

  /** Constructes the FIFO queue example DFA.
    *
    * The parameter `n` is a bound of the size of queue.
    */
  def fifo(n: Int): NDFA[Option[FIFOQueue[Atom]], FIFOAlphabet] =
    import FIFOAlphabet.{Put, Get}
    type Q = Option[FIFOQueue[Atom]]
    type A = FIFOAlphabet

    val alphabet = NSet.atoms.map(Put(_)) union NSet.atoms.map(Get(_))

    def possibleFIFOQueue(i: Int): NSet[FIFOQueue[Atom]] =
      NSet.union((0 to i).map(j => NSet.map2(NSet.atomsList(j), NSet.atomsList(i - j))(FIFOQueue(_, _))))
    val allPossibleFIFOQueue = NSet.union((0 to n).map(possibleFIFOQueue))

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

    val transitionFunction = NMap.fromSet(NSet.product(stateSet, alphabet))(transition(_, _))

    NDFA(stateSet, alphabet, initialState, acceptStateSet, transitionFunction)
