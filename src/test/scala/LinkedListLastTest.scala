import org.scalatest.{Matchers, FlatSpec}

class LinkedListLastTest extends FlatSpec with Matchers {

  "LinkedListLast" must "return Some(1)" in {
    val nthelem = LinkedListLast.findNFromLast(5, Seq(1,2,3,4,5))
    nthelem should be (Some(1))
  }
  it must "return Some(2)" in {
    val nthelem = LinkedListLast.findNFromLast(5, Seq(1,2,3,4,5,6))
    nthelem should be (Some(2))
  }

  it must "return Some(b)" in {
    val nthelem = LinkedListLast.findNFromLast(5, Seq("a","b",3,4,5,6))
    nthelem should be (Some("b"))
  }

  it must "return None" in {
    val nthelem = LinkedListLast.findNFromLast(5, Seq("a","b",3,4))
    nthelem should be (None)
  }
}
