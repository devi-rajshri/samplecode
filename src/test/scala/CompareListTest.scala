import org.scalatest.{Matchers, FlatSpec}

class CompareListTest extends FlatSpec with Matchers {

  "CompareListTest" must "return true for the following" in {
    CompareList.isListInAnother(List(1,2), List(3,2,1)) should be (true)
    CompareList.isListInAnother(List(1,2), List(1,2)) should be (true)
    CompareList.isListInAnother(List(1,2), List(2,1)) should be (true)
    CompareList.isListInAnother(Nil, List(3,2,1)) should be (true)
    CompareList.isListInAnother(List("a",1,2,"b"), List("a","b")) should be (true)
    CompareList.isListInAnother(List(1), List(3,2,1)) should be (true)

  }

  "CompareListTest" must "return false for the following" in {
    CompareList.isListInAnother(List(1,2), List("a","b")) should be (false)
    CompareList.isListInAnother(List("A"), List("a")) should be (false)
    CompareList.isListInAnother(List("a",1,2,"b"), List("a","B")) should be (false)

  }
}
