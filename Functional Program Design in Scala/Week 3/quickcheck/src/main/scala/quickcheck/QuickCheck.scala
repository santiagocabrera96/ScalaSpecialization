package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = frequency(
    (1, empty),
    (100, for {
      a <- arbitrary[Int]
      h <- frequency((1, const(empty)), (1, genHeap))
    } yield insert(a, h)))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(heap: H): List[Int] = 
      if(isEmpty(heap)) Nil
      else findMin(heap) :: toList(deleteMin(heap))
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  property("Empty inserted two elements the min is the min of the 2 elements") = forAll { (a: Int, b: Int) =>  
    findMin(insert(a, insert(b, empty))) == (a min b)
  }
  property("Insert to empty then delete min should be empty") = forAll { (a: Int) => 
    deleteMin(insert(a, empty)) == empty
  }
  property("FindMin and deleteMin recursively gives a sorted sequence") = forAll { (heap: H) =>
    toList(heap) == toList(heap).sorted
  }
  property("The min of melding two heaps is the min of the min of each heap") = forAll { (heap1: H, heap2: H) =>
    isEmpty(heap1) || isEmpty(heap2) || findMin(meld(heap1, heap2)) == (findMin(heap1) min findMin(heap2))
  }
  property("Meld is associative") = forAll { (a:H, b:H, c:H) =>
    val first = meld(meld(a, b), c)
    val second = meld(a, meld(b, c))
    toList(first) == toList(second)
  }
}
