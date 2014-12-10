package problems

object Problems {

  def last[T](seq: List[T]): T = {
    seq.foldLeft(seq.head) { (current, next) => next}
  }

  def penultimate[T](seq: List[T]): T = seq match {
    case h :: second :: Nil => h
    case h :: tail => penultimate(tail)
  }

  def nth[T](n: Int, seq: List[T]): T = (n, seq) match {
    case (0, h :: tail) => h
    case (x, h :: tail) => nth(x - 1, tail)
  }

  def length[T](seq: List[T]): Int = {
    seq.foldLeft(0) { (sum, _) => sum + 1}
  }

  def reverse[T](seq: List[T]): List[T] = {
    seq.foldLeft(List[T]()) { (acc, ele) => ele :: acc}
  }

  def isPalindrome[T](seq: List[T]): Boolean = {
    seq == reverse(seq)
  }

  def compress[T](seq: List[T]): List[T] = {
    seq.foldRight(List[T]()) { (h, acc) =>
      if (acc.isEmpty || h != acc.head) h :: acc
      else acc
    }
  }

  def pack[T](seq: List[T]): List[List[T]] = {
    if (seq.isEmpty) List(List())
    else {
      val (packed, next) = seq span (_ == seq.head)
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode[T](seq: List[T]): List[(Int, T)] = {
    pack(seq).map(pack => (pack.length, pack.head))
  }

  def encodeModified[T](seq: List[T]): List[Any] = {
    pack(seq).map(pack =>
      if (pack.size == 1) pack.head
      else (pack.length, pack.head)
    )
  }

  def decode[T](seq: List[(Int, T)]): List[T] = {
    def decodeEntry(entry: (Int, T)): List[T] = entry match {
      case (0, x) => Nil
      case (n, x) => x :: decodeEntry((n - 1, x))
    }

    seq.flatMap(decodeEntry)
  }

  def encodeDirect[T](seq: List[T]): List[(Int, T)] = seq match {
    case Nil => Nil
    case h :: tail =>
      val (prefix, suffix) = seq.span(_ == h)
      (prefix.length, h) :: encodeDirect(suffix)
  }

  def duplicate[T](seq: List[T]): List[T] = {
    seq.flatMap(List.fill(2)(_))
  }

  def duplicateN[T](n: Int, seq: List[T]): List[T] = {
    seq.flatMap(List.fill(n)(_))
  }

  def drop[T](n: Int, seq: List[T]): List[T] = {
    def dropEvery(x: Int, seq: List[T]): List[T] = (x, seq) match {
      case (_, Nil) => Nil
      case (1, h :: t) => dropEvery(n, t)
      case (_, h :: t) => h :: dropEvery(x - 1, t)
    }

    dropEvery(n, seq)
  }

  def split[T](n: Int, seq: List[T]): (List[T], List[T]) = {
    (seq.take(n), seq.drop(n))
  }

  def slice[T](from: Int, to: Int, seq: List[T]): List[T] = {
    seq.drop(from).take(to - from)
  }

  def rotate[T](n: Int, xs: List[T]): List[T] = {
    val rotation = if (n > 0) n else xs.length + n
    val (prefix, suffix) = xs.splitAt(rotation)
    suffix ::: prefix
  }

  def removeAt[T](n: Int, xs: List[T]): (List[T], T) = xs.splitAt(n) match {
    case (left, h :: right) => (left ::: right, h)
  }

  def uniqueCharacters(string: String): Boolean = {
    string.toList.groupBy(chr => chr).forall {
      case (key, chars) => chars.length == 1
    }
  }

  def isAnagram(first: String, second: String): Boolean = {
    val firstOccurrences = first.toList.groupBy(chr => chr)
    val secondOccurrences = second.toList.groupBy(chr => chr)

    firstOccurrences.forall {
      case (key, chars) => chars.length == secondOccurrences(key).length
    }
  }

  // All subsets of set
  def powerSet[T](set: Set[T]): Set[Set[T]] = {
    set.foldLeft(Set(Set[T]())) { (acc, ele) => acc ++ acc.map(_ + ele)}
  }

  // All permutations of string
  def permutations(string: String): Set[String] = {
    if (string.length == 1) {
      Set(string)
    } else {
      val result = for {
        index <- 0 until string.length
        permutation <- permutations(string.substring(0, index) + string.substring(Math.min(index + 1, string.length)))
      } yield string(index) + permutation

      result.toSet
    }
  }

  // List of only unique elements
  def distinct[T](xs: List[T]): List[T] = {
    def distinct(xs: List[T], acc: Set[T]): List[T] = xs match {
      case Nil => Nil
      case h :: t => if (!acc.contains(h)) h :: distinct(t, acc + h) else distinct(t, acc)
    }

    distinct(xs, Set[T]())
  }

  // Find if one string contains another string anagram
  def anagramSubstring(anagram: String, string: String): Boolean = {
    def occurrence(s: String): Map[Char, Int] = {
      s.groupBy(c => c).mapValues(occ => occ.length)
    }

    val anagramOccurrence = occurrence(anagram)

    (0 to (string.length - anagram.length)).exists(index =>
      anagramOccurrence == occurrence(string.substring(index, index + anagram.length)))
  }

  // Merge two lists preserving order
  def merge(first: List[Int], second: List[Int]) : List[Int] = (first, second) match {
    case (h :: t, Nil) => first
    case (Nil, h :: t) => second
    case (h1 :: t1, h2 :: t2) => if(h1 < h2) h1 :: merge(t1, second) else h2 :: merge(first, t2)
  }

  // Is given number a prime number
  def isPrime(number: Int) : Boolean = {
    if(number == 1 || number == 2) true
    else (2 to Math.sqrt(number).toInt).forall(number % _ != 0)
  }

  // Sort one list by occurrences in another list
  def sort(a: List[Int], b: List[Int]) : List[Int] = {
    val order = b.zipWithIndex.toMap

    a.sortWith((e1, e2) =>
      if(order.contains(e1) && order.contains(e2)) {
        order(e1) < order(e2)
      } else if(order.contains(e1) && !order.contains(e2)) {
        true
      } else if(!order.contains(e1) && order.contains(e2)) {
        false
      } else {
        e1 < e2
      }
    )
  }
}
