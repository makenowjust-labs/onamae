package onamae
package automaton

/** Teacher is a teacher for learning algorithms.
  *
  * A teacher provides three methods to obtain the target DFA information:
  *
  * - `Teacher#set` returns an equivariant set of alphabet.
  * - `Teacher#query` asks whether the given word is accepted or not.
  * - `Teacher#checkEquivalence` checks whether the given hypothesis DFA is equivalent to the target DFA or not.
  */
trait Teacher[A] extends Teacher.Alphabet[A], Teacher.Membership[A], Teacher.Equivalence[A]

/** Teacher partial types and utilities. */
object Teacher:

  /** Alphabet is a partial of `Teacher` to provide alphabet. */
  trait Alphabet[A]:

    /** Returns an equivariant set of alphabet of the target DFA. */
    def set: NSet[A]

  /** Membership is a partial of `Teacher` to provide a membership query function. */
  trait Membership[A]:

    /** Checks whether `word` is accepted by the target DFA or not. */
    def query(word: List[A]): Boolean

  /** Equivalence is a partial of `Teacher` to provide an equivalence query function. */
  trait Equivalence[A]:

    /** Checks whether `hypothesis` is equivalent to the target DFA or not. */
    def checkEquivalence[Q](hypothesis: NDFA[Q, A])(using Nominal[Q]): Option[List[A]]

  /** Constructs a teacher for learning `dfa`.
    *
    * This `checkEquivalence` uses the internal structure of `dfa` to find a counterexample.
    * Thus, it is a **white-box** teacher actually.
    */
  def fromDFA[Q, A](dfa: NDFA[Q, A])(using Nominal[Q], Nominal[A]): Teacher[A] = new Teacher[A]:
    override def set: NSet[A] = dfa.alphabet
    override def query(word: List[A]): Boolean = dfa.run(word)
    override def checkEquivalence[Q1](hypothesis: NDFA[Q1, A])(using Nominal[Q1]): Option[List[A]] =
      NDFA.findSepWord(dfa, hypothesis).map(_.toList)

/** LStar provides an implementation of Angluin-style algorithm for learning nominal DFAs (a.k.a. νL*).
  *
  * The algorithm is described in [Moerman, et al. (2017)], "Learning Nominal Automata".
  *
  * [Moerman, et al. (2017)]: https://dl.acm.org/doi/10.1145/3093333.3009879
  */
object LStar:
  import Teacher.{Alphabet, Membership}

  /** ObservationTable is an observation table that is a data structure used by L*.
    *
    * This contains four values:
    *
    * - `prefixSet` is an equivariant set of prefixes. Typically, it is corresponding to states of the learning DFA.
    * - `candidateSet` is an equivariant set of candidates for `prefixSet`.
    *   It is the same as `map2(prefixSet, alphabet)(_ :+ _) diff prefixSet`
    * - `separatorSet` is an equivariant set of separators. Every prefix is separated by these separators.
    *   Or, if a pair of prefixes and candidates are not separated by the separators, such a pair is called equivalent on the table.
    * - `table` is a cache of membership queries. It is managed to keep the following invariant:
    *   `table(w) = query(w)` for all `map2(prefixSet union candidateSet, separatorSet)(_ ++ _).contains(w)`.
    */
  private final case class ObservationTable[A](
      prefixSet: NSet[List[A]],
      candidateSet: NSet[List[A]],
      separatorSet: NSet[List[A]],
      table: NMap[List[A], Boolean]
  ):

    /** Checks whether a pair of prefixes (or a prefix and a candidate) is equivalent on the table. */
    private def equivalentRow(prefix1: List[A], prefix2: List[A])(using Nominal[A]): Boolean =
      prefix1 == prefix2 || NSet
        .productIterator(NSet((prefix1, prefix2)), separatorSet)
        .forall:
          case ((prefix1, prefix2), separator) => table(prefix1 ++ separator) == table(prefix2 ++ separator)

    /** Checks this observation table is closed. If unclosed, it returns an iterator of unclosed candidates.
      *
      * An **unclosed candidate** is a candidate that has no equivalent prefix on the table.
      * We say "an observation table is closed" iff every candidate are closed.
      */
    def checkClosedness(using Nominal[A]): Iterator[List[A]] =
      candidateSet.iterator.filter: candidate =>
        !NSet.productIterator(NSet(candidate), prefixSet).exists(equivalentRow(_, _))

    /** Check this observation table is consistent. If inconsistent, it returns an iterator of new separators.
      *
      * We say "an observation table is consistent" iff, for every pair of prefixes equivalent on the table,
      * every pair of candidate of their prefixes are also equivalent.
      */
    def checkConsistency(using Nominal[A], Alphabet[A]): Iterator[List[A]] =
      val ListA = summon[Nominal[List[A]]]
      val eqPairIter = NSet
        .productIterator(prefixSet, prefixSet)
        .filter: (p1, p2) =>
          p1 != p2 && ListA.support(p1) <= ListA.support(p2) && equivalentRow(p1, p2)
      val eqPairSet = NSet.from(eqPairIter)
      val newSeparatorSet = NSet
        .productIterator(eqPairSet, NSet.product(summon[Alphabet[A]].set, separatorSet))
        .filter:
          case ((p1, p2), (a, separator)) =>
            table(p1 ++ (a :: separator)) != table(p2 ++ (a :: separator))
        .map { case (_, (a, separator)) => a :: separator }
      newSeparatorSet

    /** Fills `table` against `wordSet` and returns the updated observation table. */
    private def updateTable(
        wordSet: NSet[(List[A], List[A])]
    )(using A: Nominal[A], m: Membership[A]): ObservationTable[A] =
      val newEntries = wordSet.toSeq.iterator.map: (prefix, separator) =>
        val word = prefix ++ separator
        word -> table.get(word).getOrElse(m.query(prefix ++ separator))
      copy(table = table ++ NMap(newEntries.toSeq*))

    /** Promotes `candidate` to a prefix and returns the updated observation table. */
    def promote(candidate: List[A])(using Nominal[A], Alphabet[A], Membership[A]): ObservationTable[A] =
      val newCandidateSet = NSet.map2(NSet(candidate), summon[Alphabet[A]].set)(_ :+ _)
      copy(
        prefixSet = prefixSet + candidate,
        candidateSet = candidateSet diff NSet(candidate) union newCandidateSet
      ).updateTable(NSet.product(newCandidateSet, separatorSet))

    /** Adds `prefix` to `prefixSet` and returns the updated observation table. */
    def addPrefix(prefix: List[A])(using Nominal[A], Alphabet[A], Membership[A]): ObservationTable[A] =
      if prefixSet.contains(prefix) then this
      else if candidateSet.contains(prefix) then promote(prefix)
      else
        val newCandidateSet = NSet.map2(NSet(prefix), summon[Alphabet[A]].set)(_ :+ _)
        copy(
          prefixSet = prefixSet + prefix,
          candidateSet = candidateSet union newCandidateSet
        ).updateTable(NSet.product(NSet(prefix) union newCandidateSet, separatorSet))

    /** Adds `separator` to `separatorSet` and returns the updated observation table. */
    def addSeparator(separator: List[A])(using Nominal[A], Membership[A]): ObservationTable[A] =
      if separatorSet.contains(separator) then this
      else
        copy(separatorSet = separatorSet + separator)
          .updateTable(NSet.product(prefixSet union candidateSet, NSet(separator)))

    /** Creates the corresponding hypothesis DFA of `this`. */
    def makeHypothesis(using Nominal[A], Alphabet[A]): NDFA[NSet.EquivalentClass, A] =
      val (quot, ks) = prefixSet.quotient(equivalentRow(_, _))
      val candidatePairIter = NSet
        .productIterator(candidateSet, prefixSet)
        .filter(equivalentRow(_, _))
        .map((c, p) => c -> quot(p))
      val candidateQuot = NMap.from(candidatePairIter)
      val transitionIter = NSet
        .productIterator(prefixSet, summon[Alphabet[A]].set)
        .map: (prefix, a) =>
          val nextWord = prefix :+ a
          val nextQ = if candidateSet.contains(nextWord) then candidateQuot(nextWord) else quot(nextWord)
          (quot(prefix), a) -> nextQ
      NDFA(
        ks,
        summon[Alphabet[A]].set,
        quot(Nil),
        NSet.from(prefixSet.iterator.filter(table(_)).map(quot(_))),
        NMap.from(transitionIter)
      )

  /** Observation utilities. */
  private object ObservationTable:

    /** Returns the empty observation table. */
    def empty[A](using Nominal[A], Alphabet[A], Membership[A]): ObservationTable[A] =
      ObservationTable(NSet.empty, NSet.empty, NSet(Nil), NMap.empty).addPrefix(Nil)

  /** Run νL* algorithm for learning the target DFA of `teacher`. */
  def learn[A](teacher: Teacher[A])(using Nominal[A]): NDFA[NSet.EquivalentClass, A] =
    given Teacher[A] = teacher

    var obs = ObservationTable.empty[A]
    var result: Option[NDFA[NSet.EquivalentClass, A]] = None
    while result.isEmpty do
      var continue = false

      val newPrefixIter = obs.checkClosedness
      if newPrefixIter.nonEmpty then
        val newPrefix = newPrefixIter.next()
        obs = obs.promote(newPrefix)
        continue = true

      if !continue then
        val newSeparatorIter = obs.checkConsistency
        if newSeparatorIter.nonEmpty then
          val newSeparator = newSeparatorIter.next()
          obs = obs.addSeparator(newSeparator)
          continue = true

      if !continue then
        val hypothesis = obs.makeHypothesis
        teacher.checkEquivalence(hypothesis) match
          case Some(cex) =>
            // Every suffixes of the counterexample are added as a separator to the observation table.
            // It is different to the usual Angluin's L*, but it is known that the performance is better.
            obs = cex.tails.foldLeft(obs)(_.addSeparator(_))
          case None =>
            result = Some(hypothesis)

    result.get
