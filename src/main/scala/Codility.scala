/*A zero-indexed array A consisting of N integers is given. Rotation of the array means that each element is shifted right by one index, and the last element of the array is also moved to the first place.

For example, the rotation of array A = [3, 8, 9, 7, 6] is [6, 3, 8, 9, 7]. The goal is to rotate array A K times; that is, each element of A will be shifted to the right by K indexes.

  Write a function:

object Solution { def solution(a: Array[Int], k: Int): Array[Int] }

that, given a zero-indexed array A consisting of N integers and an integer K, returns the array A rotated K times.

  For example, given array A = [3, 8, 9, 7, 6] and K = 3, the function should return [9, 7, 6, 3, 8].

Assume that:

  N and K are integers within the range [0..100];
each element of array A is an integer within the range [−1,000..1,000].
In your solution, focus on correctness. The performance of your solution will not be the focus of the assessment.*/

object ArrayShift {
  def solution(a: Array[Int], k: Int): Array[Int] = {

    if(a.isEmpty || k ==0 || k % a.length == 0) {
      a
    }else {
      val correctK = k % a.length
       val first = a.drop(a.length- correctK)
      val last = a.take(a.length -correctK)
      first ++ last

    }

  }
}

/**
  * A non-empty zero-indexed array A consisting of N integers is given. The array contains an odd number of elements, and each element of the array can be paired with another element that has the same value, except for one element that is left unpaired.
  **
  * For example, in array A such that:
  **
  *A[0] = 9  A[1] = 3  A[2] = 9
  *A[3] = 3  A[4] = 9  A[5] = 7
  *A[6] = 9
  *the elements at indexes 0 and 2 have value 9,
  *the elements at indexes 1 and 3 have value 3,
  *the elements at indexes 4 and 6 have value 9,
  *the element at index 5 has value 7 and is unpaired.
  *Write a function:
  **
  *class Solution { public int solution(int[] A); }
  **
 *that, given an array A consisting of N integers fulfilling the above conditions, returns the value of the unpaired element.
  **
 *For example, given array A such that:
  **
 *A[0] = 9  A[1] = 3  A[2] = 9
  *A[3] = 3  A[4] = 9  A[5] = 7
  *A[6] = 9
*the function should return 7, as explained in the example above.
  **
 *Assume that:
 **
 *N is an odd integer within the range [1..1,000,000];
*each element of array A is an integer within the range [1..1,000,000,000];
*all but one of the values in A occur an even number of times.
*Complexity:
 **
 *expected worst-case time complexity is O(N);
*expected worst-case space complexity is O(1), beyond input storage (not counting the storage required for input arguments).
*Elements of input arrays can be modified.
  */

object FindUnpaired {

  /*def findUnpaired(currentlist:Array[Int], buffer:Array[Int]): Int ={
    currentlist match {
      case x if x.isEmpty =>
        if(buffer.length == 1) buffer.head
        else
          {
            println("we have an error"+buffer.toString)
            0
          }
      case x =>
        val newbuffer = if(buffer.contains(currentlist.head))
          buffer.filterNot(_.equals(currentlist.head))
        else buffer :+ currentlist.head
        findUnpaired(currentlist.tail,newbuffer)
    }

  }

  def solution(a: Array[Int]): Int = {
    val list = a.sorted

    findUnpaired(list.tail,Array(list.head))
  }*/


  def solution(A: Array[Int]): Int = {
    val a = A.sorted
    var buffer = Array(a(0))
    for ( i <- 1 until a.length ) yield {
      if(buffer.contains(a(i))){
        buffer = buffer.filterNot(_.equals(a(i)))
      }else{
        buffer = buffer :+ a(i)
      }
    }

    buffer(0)

  }
}

/*
A small frog wants to get to the other side of the road. The frog is currently located at position X and wants to get to a position greater than or equal to Y. The small frog always jumps a fixed distance, D.

Count the minimal number of jumps that the small frog must perform to reach its target.

Write a function:

int solution(int X, int Y, int D);
that, given three integers X, Y and D, returns the minimal number of jumps from position X to a position equal to or greater than Y.

For example, given:

  X = 10
  Y = 85
  D = 30
the function should return 3, because the frog will be positioned as follows:

after the first jump, at position 10 + 30 = 40
after the second jump, at position 10 + 30 + 30 = 70
after the third jump, at position 10 + 30 + 30 + 30 = 100
Assume that:

X, Y and D are integers within the range [1..1,000,000,000];
X ≤ Y.
Complexity:

expected worst-case time complexity is O(1);
expected worst-case space complexity is O(1).
 */

object FrogJmp{

  def solution(x: Int, y: Int, d: Int): Int = {
    val minjumps = (y-x) / d
    val minumps10 = ((y -x)*10)/d
    val decimalpoints = minumps10 - minjumps*10
    if(decimalpoints != 0 ) minjumps +1 else minjumps

  }
}

object OddNumbers {
  def findGradeRound(in:Array[Int]): Array[Int] = {
    in.map { _ match {
      case g if g % 5 == 0 => g
      case g if g <38 => g
      case g if (g+1) % 5 ==0 => g+1
      case g if (g+2) % 5 ==0 => g+2
      case g => g
      }
    }
  }
}
