import org.scalatest.{Matchers, FlatSpec}


class SolutionTest extends FlatSpec with Matchers {
    "TapeEquilibrium" should "function correctly" in {
      TapeEquilibrium.tapeEquilibrium(Array(3,1,2,4,3)) should be (1)
      TapeEquilibrium.tapeEquilibrium(Array(-1000, 1000)) should be (2000)
    }

    "Solution" should "match correctly " in {
      Solution.solution("())") should be (1)
      Solution.solution("()") should be (0)
      Solution.solution("{()}[]") should be (0)
      Solution.solution("[{()}]") should be (0)
      Solution.solution("{[()}]") should be (1)
    }
}
