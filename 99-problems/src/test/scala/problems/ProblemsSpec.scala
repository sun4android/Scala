package problems

import org.scalatest.{FlatSpec, Matchers}

class ProblemsSpec extends FlatSpec with Matchers {

  "P01" should "Find the last element of a list" in {
    Problems.last(List(1)) shouldEqual 1
    Problems.last(List(1, 2, 3)) shouldEqual 3
  }

  "P02" should "Find the last but one element of a list" in {
    Problems.penultimate(List(1, 2, 3)) shouldEqual 2
  }

  "P03" should "Find the Kth element of a list" in {
    Problems.nth(2, List(1, 2, 3, 4, 5)) shouldEqual 3
  }

  "P04" should "Find the number of elements of a list" in {
    Problems.length(List(1, 2, 3)) shouldEqual 3
    Problems.length(List(1)) shouldEqual 1
  }

  "P05" should "Reverse a list" in {
    Problems.reverse(List(1, 2, 3)) shouldEqual List(3, 2, 1)
  }

  "P06" should "isPalindrome" in {
    Problems.isPalindrome(List(1, 2, 1)) shouldEqual true
    Problems.isPalindrome(List(1, 2, 3)) shouldEqual false
  }

  "P08" should "Eliminate consecutive duplicates of list elements" in {
    Problems.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual
      List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "P09" should "Pack consecutive duplicates of list elements into sublists" in {
    Problems.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "P10" should "Run-length encoding of a list" in {
    Problems.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual
      List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  "P11" should "Only elements with duplicates are transferred as (N, E) terms." in {
    Problems.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual
      List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
  }

  "P12" should "Duplicate the elements of a list." in {
    Problems.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldEqual
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  "P13" should "Run-length encoding of a list (direct solution)" in {
    Problems.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual
      List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  "P14" should "Run-length encoding of a list (direct solution)" in {
    Problems.duplicate(List('a, 'b, 'c, 'c, 'd)) shouldEqual
      List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }

  "P15" should "Duplicate the elements of a list a given number of times" in {
    Problems.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) shouldEqual
      List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "P16" should "Drop every Nth element from a list" in {
    Problems.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual
      List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "P17" should "Split a list into two parts" in {
    Problems.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual
      (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "P18" should "Extract a slice from a list" in {
    Problems.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual
      List('d, 'e, 'f, 'g)
  }

  "P19" should "Rotate a list N places to the left" in {
    Problems.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual
      List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

    Problems.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual
      List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }

  "P20" should "Remove the Kth element from a list" in {
    Problems.removeAt(1, List('a, 'b, 'c, 'd)) shouldEqual
      (List('a, 'c, 'd), 'b)
  }

  "PowerSet" should "contain all subsets of set" in {
    Problems.powerSet(Set(1, 2, 3)) shouldEqual Set(Set(), Set(1), Set(2), Set(3), Set(1, 3), Set(1, 2), Set(2, 3), Set(1, 2, 3))
  }

  "Permutations" should "contain all permutations of given word" in {
    Problems.permutations("dom") shouldEqual Set("mod", "odm", "dom", "mdo", "omd", "dmo")
  }

  "Distinct list" should "contain unique elements" in {
    Problems.distinct(List(1, 2, 2, 3, 1, 4, 2, 3, 1, 5)) shouldEqual List(1, 2, 3, 4, 5)
  }

  "Any anagram of given string" should "appear in next string" in {
    Problems.anagramSubstring("xyzk", "afdgzykxsldfm") shouldEqual true
    Problems.anagramSubstring("xyzk", "afdgzyxsldfm") shouldEqual false
  }

  "Merge list" should "contain all elements and preserve ordering" in {
    Problems.merge(List(1, 3, 5, 7), List(2, 4, 6, 8, 10, 12)) shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 10, 12)
  }

  "Sort" should "sort elements based on another list of elements" in {
    Problems.sort(List(4, 2, 7, 6, 8, 9, 1, 3, 2, 5, 6), List(6, 3, 4, 1)) shouldEqual List(6, 6, 3, 4, 1, 2, 2, 5, 7, 8, 9)
  }

}
