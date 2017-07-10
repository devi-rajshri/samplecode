/*
1. Write a function that takes three sides of a triangle and answers if it's equilateral, isosceles, or scalene.

2. For a single-linked (forward only) list write a function that returns 5th element from the end of the list. The list can only be walked once (length or size of this list cannot be used).

3. Given two lists, write a function that answers if all elements of one list are in the other.
 */

case class Triangle (sideA:Double, sideB: Double, sideC:Double) {
  def isIsoceles :Boolean = (sideA == sideB ) || (sideB == sideC) || (sideA == sideC)
  def isEquilateral :Boolean = sideA == sideB && sideB == sideC
  // we can define all three and use the triangle class itself for the test
  // However since the test needs a single method to tell it the kind of triangle , the following 2 methods are redundant
  def isScalene :Boolean = (sideA != sideB) && (sideB != sideC) && (sideA != sideC)
  def isScaleneAlt:Boolean = !isIsoceles && !isEquilateral
}

object TriangleType extends Enumeration {
  val Isoceles = Value("Isoceles")
  val Equilateral = Value("Equilateral")
  val Scalene = Value("Scalene")

}

object Triangle {

  def testTriangle(triangle:Triangle):TriangleType.Value = {
    triangle match {
      case t if t.isEquilateral => TriangleType.Equilateral
      case t if t.isIsoceles => TriangleType.Isoceles
      case _ => TriangleType.Scalene
    }
  }


}


object LinkedListLast {

  def findNFromLast(n:Int, list:Seq[Any]):Option[Any] = {
    val revertedList = list.reverse
    nthElem(0,n,revertedList)
  }

  def nthElem(pos:Int, n:Int, list:Seq[Any]):Option[Any] ={
    if(list.isEmpty) None
    else {
      if (pos + 1 == n) list.headOption
      else nthElem(pos + 1, n, list.tail)
    }
  }

}


object CompareList {

  def isListInAnother[A](listA:List[A], listB: List[A]):Boolean = {
    if(listA.isEmpty || listB.isEmpty || listA.containsSlice(listB) || listB.containsSlice(listA)) true
    else {
      val intersection = listA.intersect(listB)
      listA.filterNot(intersection.contains).isEmpty || listB.filterNot(intersection.contains).isEmpty
    }
  }

}

