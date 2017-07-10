
/*
A non-empty zero-indexed array A consisting of N integers is given. Array A represents numbers on a tape.

Any integer P, such that 0 < P < N, splits this tape into two non-empty parts: A[0], A[1], ..., A[P − 1] and A[P], A[P + 1], ..., A[N − 1].

The difference between the two parts is the value of: |(A[0] + A[1] + ... + A[P − 1]) − (A[P] + A[P + 1] + ... + A[N − 1])|

In other words, it is the absolute difference between the sum of the first part and the sum of the second part.

For example, consider array A such that:

  A[0] = 3
  A[1] = 1
  A[2] = 2
  A[3] = 4
  A[4] = 3
We can split this tape in four places:

P = 1, difference = |3 − 10| = 7
P = 2, difference = |4 − 9| = 5
P = 3, difference = |6 − 7| = 1
P = 4, difference = |10 − 3| = 7
Write a function:

int solution(int A[], int N);

that, given a non-empty zero-indexed array A of N integers, returns the minimal difference that can be achieved.

For example, given:

  A[0] = 3
  A[1] = 1
  A[2] = 2
  A[3] = 4
  A[4] = 3
the function should return 1, as explained above.

Assume that:

N is an integer within the range [2..100,000];
each element of array A is an integer within the range [−1,000..1,000].
Complexity:

expected worst-case time complexity is O(N);
expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
Elements of input arrays can be modified.
 */

object TapeEquilibrium {
  def tapeEquilibrium(A: Array[Int]): Int = {
    var sumA:Int = A(0)
    var sumB:Int = A.tail.sum

    var min = math.abs(sumA - sumB)

    for (i <- 1 until A.length -1 ) yield {
      sumA = sumA + A(i)
      sumB = sumB - A(i)
      min = math.min(min, math.abs(sumA - sumB))
    }
    min
  }


  /*protected def findMinInEquilibrium(firstSum: Int, lastSum: Int, A: Array[Int], min: Int): Int = {
    A match {
      case A if A.tail.isEmpty => min
      case _ =>
        val newfirstSum = firstSum + A.head
        val newLastSum = lastSum - A.head
        val newmin = math.abs(newfirstSum - newLastSum)

        findMinInEquilibrium(newfirstSum, newLastSum, A.tail, if (min > newmin) newmin else min)
    }

  }*/


}

/*
A zero-indexed array A consisting of N different integers is given. The array contains integers in the range [1..(N + 1)], which means that exactly one element is missing.

Your goal is to find that missing element.

Write a function:

int solution(int A[], int N);

that, given a zero-indexed array A, returns the value of the missing element.

For example, given array A such that:

  A[0] = 2
  A[1] = 3
  A[2] = 1
  A[3] = 5
the function should return 4, as it is the missing element.

Assume that:

N is an integer within the range [0..100,000];
the elements of A are all distinct;
each element of array A is an integer within the range [1..(N + 1)].
Complexity:

expected worst-case time complexity is O(N);
expected worst-case space complexity is O(1), beyond input storage (not counting the storage required for input arguments).
Elements of input arrays can be modified.
 */


object FindMissingNum {
  def findMissing(A: Array[Int]): Int = {
    // write your code in Scala 2.10
    //to do: use merge sort instead of scala's default sort which is O(n*2)
    findMissing(A.sorted,0)
  }
  def findMissing(A:Array[Int],n:Int):Int = {
    if(A.isEmpty || A.head != n+1) n+1
    else findMissing(A.tail, n+1)
  }
}

/*
A small frog wants to get to the other side of a river. The frog is currently located at position 0, and wants to get to position X. Leaves fall from a tree onto the surface of the river.

You are given a non-empty zero-indexed array A consisting of N integers representing the falling leaves. A[K] represents the position where one leaf falls at time K, measured in seconds.

The goal is to find the earliest time when the frog can jump to the other side of the river. The frog can cross only when leaves appear at every position across the river from 1 to X. You may assume that the speed of the current in the river is negligibly small, i.e. the leaves do not change their positions once they fall in the river.

For example, you are given integer X = 5 and array A such that:

  A[0] = 1
  A[1] = 3
  A[2] = 1
  A[3] = 4
  A[4] = 2
  A[5] = 3
  A[6] = 5
  A[7] = 4
In second 6, a leaf falls into position 5. This is the earliest time when leaves appear in every position across the river.

Write a function:

object Solution { def solution(X: Int, A: Array[Int]): Int }

that, given a non-empty zero-indexed array A consisting of N integers and integer X, returns the earliest time when the frog can jump to the other side of the river.

If the frog is never able to jump to the other side of the river, the function should return −1.

For example, given X = 5 and array A such that:

  A[0] = 1
  A[1] = 3
  A[2] = 1
  A[3] = 4
  A[4] = 2
  A[5] = 3
  A[6] = 5
  A[7] = 4
the function should return 6, as explained above.

Assume that:

N and X are integers within the range [1..100,000];
each element of array A is an integer within the range [1..X].
Complexity:

expected worst-case time complexity is O(N);
expected worst-case space complexity is O(X), beyond input storage (not counting the storage required for input arguments).
Elements of input arrays can be modified.
 */

object Solution1 {
  def solution(X: Int, A: Array[Int]): Int = {
    // write your code in Scala 2.10
    earliestTime(0,Range.inclusive(1,X,1).toList,A)
  }

  def earliestTime(currentTime:Int, positionsToCover:List[Int], posAtTime:Array[Int]):Int = {
    if(posAtTime.isEmpty) -1
    else {
      val newPosToCover = positionsToCover diff List(posAtTime.head)
      if (newPosToCover.isEmpty) currentTime
      else
        earliestTime(currentTime+1,newPosToCover,posAtTime.tail)
    }
  }
}

object Solution2 {
  def solution(N: Int, A: Array[Int]): Array[Int] = {
    // write your code in Scala 2.10
    Counter.apply(N)
    for(a <- A) yield {
      if(a > 0 && a <= N) Counter.counters(a-1).inc()
      else if(a == N+1) Counter.maxCounter()
    }
    Counter.counters.map(_.getCount).toArray
  }
}

class Counter(){
  private var count = 0
  def inc():Unit = {
    count = count+1
  }
  def inc(newValue:Int):Unit = {
    count = newValue
  }
  def getCount :Int = count
}

object Counter {
  var counters: Seq[Counter] = Nil
  def maxCounter():Unit = {
    val max = counters.map(_.count).max
    counters.foreach(_.inc(max))
  }
  def apply(n:Int) = {
     counters = for(i<- 0 until n-1) yield {new Counter()}
  }
}

object Solution {
  val pairs = Seq(Pair('(',')'),Pair('{','}'),Pair('[',']'))
  def solution(S: String): Int = {
     val closingChars = S.foldLeft(List[Char]()){ case ( stack:List[Char],elem:Char) =>
      if(isOpen(elem)){
        elem :: stack
      } else {
        if(isClose(elem) && matches(stack.headOption,elem)){
          stack.tail
        }else {
          elem :: stack
        }
      }

    }
    if(closingChars.isEmpty) 0 else 1
  }

  def isOpen(char:Char) = {
    pairs.exists(_.isOpening(char))
  }
  def isClose(char:Char) = {
    pairs.exists(_.isClosing(char))

  }
  def matches(open:Option[Char], close:Char) = {
    pairs.exists(_.matches(open,close))
  }
}

case class Pair(open:Char, close:Char){
  def isOpening(char:Char) = char == open
  def isClosing(char:Char) = char == close
  def matches(open:Option[Char], close:Char) = if(open.isEmpty) false else { isOpening(open.get) && isClosing(close)}
}




