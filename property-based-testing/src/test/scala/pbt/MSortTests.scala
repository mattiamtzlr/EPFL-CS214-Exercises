package pbt

import scala.language.adhocExtensions
import org.scalacheck.*
import Gen.*
import Prop.*

object MSortSpecification extends Properties("MSort"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(500)

/*   property("List length is less than or equal to 1") = forAll {
    (l: List[Int]) => mSort(l).length <= 1
  } */

/*   property("List length is less than or equal to 2") = forAll {
    (l: List[Int]) => mSort(l).length <= 2
  } */

  property("sortedAscendingOrder") = forAll {
    (l: List[Int]) => isSortedAscending(mSort(l))
  }

  property("lengthsEqual") = forAll {
    (l: List[Int]) => mSort(l).length == l.length
  }

/*   property("Length of sorted list is less than or equal to that of the original list") = forAll {
    (l: List[Int]) => mSort(l).length <= l.length
  } */