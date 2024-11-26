package onamae
package automaton

object LStar:
  trait Alphabet[A]:
    def set: NSet[A]

  trait Membership[A]:
    def query(word: List[A]): Boolean

  trait Equivalence[A]:
    def checkEquivalence[Q](hypothesis: NDFA[Q, A])(using Nominal[Q]): Option[List[A]]

  trait Teacher[A] extends Alphabet[A], Membership[A], Equivalence[A]

  object Teacher:
    def fromDFA[Q, A](dfa: NDFA[Q, A])(using Nominal[Q], Nominal[A]): Teacher[A] = new Teacher[A]:
      override def set: NSet[A] = dfa.alphabet
      override def query(word: List[A]): Boolean = dfa.run(word)
      override def checkEquivalence[Q1](hypothesis: NDFA[Q1, A])(using Nominal[Q1]): Option[List[A]] =
        NDFA.findSepWord(dfa, hypothesis).map(_.toList)

  final case class ObservationTable[A](
      prefixSet: NSet[List[A]],
      candidateSet: NSet[List[A]],
      separatorSet: NSet[List[A]],
      table: NMap[List[A], Boolean]
  ):
    def equivalentRow(prefix1: List[A], prefix2: List[A])(using Nominal[A]): Boolean =
      prefix1 == prefix2 || NSet
        .productIterator(NSet((prefix1, prefix2)), separatorSet)
        .forall:
          case ((prefix1, prefix2), separator) => table(prefix1 ++ separator) == table(prefix2 ++ separator)

    def checkClosedness(using Nominal[A]): Iterator[List[A]] =
      candidateSet.iterator.filter(candidate =>
        !NSet.productIterator(NSet(candidate), prefixSet).exists(equivalentRow(_, _))
      )

    def checkConsistency(using Nominal[A], Alphabet[A]): Iterator[List[A]] =
      val ListA = summon[Nominal[List[A]]]
      val eqPairSet = NSet
        .product(prefixSet, prefixSet)
        .filter: (p1, p2) =>
          p1 != p2 && ListA.support(p1) <= ListA.support(p2) && equivalentRow(p1, p2)
      val newSeparatorSet = NSet
        .productIterator(eqPairSet, summon[Alphabet[A]].set)
        .flatMap:
          case ((p1, p2), a) =>
            NSet
              .productIterator(NSet((p1, p2)), NSet.product(NSet(a), separatorSet).map(_ :: _))
              .filter:
                case ((p1, p2), separator) => table(p1 ++ separator) != table(p2 ++ separator)
              .map(_._2)
      newSeparatorSet

    def updateTable(wordSet: NSet[(List[A], List[A])])(using A: Nominal[A], m: Membership[A]): ObservationTable[A] =
      val newEntries = wordSet.toSeq.iterator.map: (prefix, separator) =>
        val word = prefix ++ separator
        word -> table.get(word).getOrElse(m.query(prefix ++ separator))
      copy(table = table ++ NMap(newEntries.toSeq*))

    def promote(candidate: List[A])(using Nominal[A], Alphabet[A], Membership[A]): ObservationTable[A] =
      val newCandidateSet = NSet.map2(NSet(candidate), summon[Alphabet[A]].set)(_ :+ _)
      ObservationTable(
        prefixSet + candidate,
        candidateSet diff NSet(candidate) union newCandidateSet,
        separatorSet,
        table
      ).updateTable(NSet.product(newCandidateSet, separatorSet))

    def addPrefix(prefix: List[A])(using Nominal[A], Alphabet[A], Membership[A]): ObservationTable[A] =
      if prefixSet.contains(prefix) then this
      else if candidateSet.contains(prefix) then promote(prefix)
      else
        val newCandidateSet = NSet.map2(NSet(prefix), summon[Alphabet[A]].set)(_ :+ _)
        ObservationTable(
          prefixSet + prefix,
          candidateSet union newCandidateSet,
          separatorSet,
          table
        ).updateTable(NSet.product(NSet(prefix) union newCandidateSet, separatorSet))

    def addSeparator(separator: List[A])(using Nominal[A], Membership[A]): ObservationTable[A] =
      if separatorSet.contains(separator) then this
      else
        copy(separatorSet = separatorSet + separator)
          .updateTable(NSet.product(prefixSet union candidateSet, NSet(separator)))

    def makeHypothesis(using Nominal[A], Alphabet[A]): NDFA[NSet.EquivalentClass, A] =
      val (quot, ks) = prefixSet.quotient(equivalentRow(_, _))
      val candidatePairSet = NSet
        .product(candidateSet, prefixSet)
        .filter(equivalentRow(_, _))
        .map((c, p) => c -> quot(p))
      val candidateQuot = NMap(candidatePairSet.toSeq*)
      val transitions = NSet.map2(prefixSet, summon[Alphabet[A]].set): (prefix, a) =>
        val nextWord = prefix :+ a
        val nextQ = if candidateSet.contains(nextWord) then candidateQuot(nextWord) else quot(nextWord)
        (quot(prefix), a) -> nextQ
      NDFA(
        ks,
        summon[Alphabet[A]].set,
        quot(Nil),
        prefixSet.filter(table(_)).map(quot(_)),
        NMap(transitions.toSeq*)
      )

  object ObservationTable:
    def empty[A](using Nominal[A], Alphabet[A], Membership[A]): ObservationTable[A] =
      ObservationTable(NSet.empty, NSet.empty, NSet(Nil), NMap.empty).addPrefix(Nil)

  def learn[A](teacher: Teacher[A])(using Nominal[A]): NDFA[NSet.EquivalentClass, A] =
    given Teacher[A] = teacher

    var obs = ObservationTable.empty[A]
    var result: Option[NDFA[NSet.EquivalentClass, A]] = None
    while result.isEmpty do
      var continue = false

      var newSeparatorSet = obs.checkConsistency
      if newSeparatorSet.nonEmpty then
        while newSeparatorSet.nonEmpty do
          obs = newSeparatorSet.foldLeft(obs)(_.addSeparator(_))
          newSeparatorSet = obs.checkConsistency
        continue = true

      if !continue then
        var newPrefixSet = obs.checkClosedness
        if newPrefixSet.nonEmpty then
          while newPrefixSet.nonEmpty do
            obs = newPrefixSet.foldLeft(obs)(_.promote(_))
            newPrefixSet = obs.checkClosedness
          continue = true

      if !continue then
        val hypothesis = obs.makeHypothesis
        teacher.checkEquivalence(hypothesis) match
          case Some(cex) =>
            obs = cex.tails.foldLeft(obs)(_.addSeparator(_))
          case None =>
            result = Some(hypothesis)

    result.get
