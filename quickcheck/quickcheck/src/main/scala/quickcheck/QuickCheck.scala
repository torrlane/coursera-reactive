package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == a.min(b)
  }
  property("addDel") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    h2 == empty
  }
  property("minHeaps") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true else {
      val h = meld(h1, h2)
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      findMin(h) == m1.min(m2)
    }
  }
  property("meldRetainsOrder") = forAll {(h1: H, h2: H) => 
    if (isEmpty(h1) || isEmpty(h2)) true else {
      val h = meld(h1, h2)
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      isSorted(findMin(h), deleteMin(h))
    }
  }
  property("recalculatesSizeAfterMeld") = forAll { (h1: H, h2: H) => 
    size(meld(h1, h2)) == size(h1) + size(h2)
  }
  property("inputIsSameAsOutput") = forAll { (a: Int, b: Int, c: Int, d: Int) => 
    val h = insert(a, insert(b, insert(c, insert(d, empty))))
    findMin(h) == a.min(b).min(c).min(d)
    findMin(deleteMin(deleteMin(deleteMin(h)))) == a.max(b).max(c).max(d) 
  }
  property("decrementsSizeAfterDelete") = forAll { (h: H) => 
    if (isEmpty(h)) true else size(deleteMin(h)) == size(h) - 1
  }
  property("does not dedupe on insert") = forAll { (h: H, a: Int) => 
    size(insert(a, insert(a, h))) == size(h) + 2
  }
  property("does not dedupe on delete") = forAll { (h: H, a: Int) => 
    if (isEmpty(h)) true else {
      size(deleteMin(insert(findMin(h), h))) == size(h)
    }
  }
  property("sortedDeletes") = forAll { h: H =>
    if (isEmpty(h)) true else isSorted(findMin(h), deleteMin(h))
  }

  def isSorted(min: Int, h: H): Boolean = {
    if (isEmpty(h)) true
    else {
      if (findMin(h) < min) false else isSorted(findMin(h), deleteMin(h))
    }
  }
  
  def size(h: H): Int = {
    def sizeHelper(count: Int, h: H): Int = {
      if (isEmpty(h)) count else sizeHelper(count+1, deleteMin(h))
    }
    sizeHelper(0, h)
  }

  lazy val genHeap: Gen[H] = for {
    isEmpty <- arbitrary[Boolean]
    heap <- if (isEmpty) const(empty) else nonEmpty
  } yield heap

  def nonEmpty: Gen[H] = for {
    a <- arbitrary[Int]
    h1 <- genHeap
  } yield insert(a, h1)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
