import org.scalatest.{FlatSpec, Matchers}

class CodilityTest extends FlatSpec with Matchers {
  "ArrayShift" must "return correct values"  in {
    ArrayShift.solution(Array(),0) should be (Array())
    ArrayShift.solution(Array(-9,0),2) should be (Array(-9,0))
    ArrayShift.solution(Array(3, 8, 9, 7, 6),1) should be (Array(6, 3, 8, 9, 7))
    ArrayShift.solution(Array(3, 8, 9, 7, 6),2) should be (Array( 7, 6, 3, 8,9))

    ArrayShift.solution(Array(3, 8, 9, 7, 6),3) should be (Array(9, 7, 6, 3, 8))
    ArrayShift.solution(Array(3, 8, 9, 7, 6),4) should be (Array(8,9, 7, 6, 3))
    ArrayShift.solution(Array(3, 8, 9, 7, 6),5) should be (Array(3, 8, 9, 7, 6))

    ArrayShift.solution(Array(5, -1000),1) should be (Array(-1000,5))

    ArrayShift.solution(Array(1, 1, 2, 3, 5),2) should be (Array(3, 5, 1, 1, 2))
    ArrayShift.solution(Array(1, 1, 2, 3, 5),42) should be (Array(3, 5, 1, 1, 2))

  }

  "FindUnpaired" must "return unpaired value"  in {
    FindUnpaired.solution(Array(9,3,9,3,9,7,9)) should be (7)
  }

  "Frogjumps" must "return no of jumps value"  in {
    FrogJmp.solution(10, 85,30) should be (3)
  }

}
