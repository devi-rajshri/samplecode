import org.scalatest.FlatSpec
import org.scalatest.Matchers


class TriangleTest extends FlatSpec with Matchers {

  "12,12,12" must "be an equilateral triangle and an isoceles triangle" in {
    val triangle = Triangle(12,12,12)
    triangle.isEquilateral should be (true)
    triangle.isIsoceles should be (true) // an equilateral triangle is also an isoceles triangle
    triangle.isScalene should be (false)
    triangle.isScaleneAlt should be (false)
    Triangle.testTriangle(triangle) should be (TriangleType.Equilateral)
  }

  "12,13,12" must "be an isoceles triangle" in {
    val triangle = Triangle(12,13,12)
    triangle.isEquilateral should be (false)
    triangle.isIsoceles should be (true)
    triangle.isScalene should be (false)
    triangle.isScaleneAlt should be (false)
    Triangle.testTriangle(triangle) should be (TriangleType.Isoceles)
  }

  "11,13,12" must "be a scalene triangle" in {
    val triangle = Triangle(11,13,12)
    triangle.isEquilateral should be (false)
    triangle.isIsoceles should be (false)
    triangle.isScalene should be (true)
    triangle.isScaleneAlt should be (true)
    Triangle.testTriangle(triangle) should be (TriangleType.Scalene)
  }

  "11.5,13.2,12.1" must "be a scalene triangle" in {
    val triangle = Triangle(11.5,13.2,12.1)
    triangle.isEquilateral should be (false)
    triangle.isIsoceles should be (false)
    triangle.isScalene should be (true)
    triangle.isScaleneAlt should be (true)
    Triangle.testTriangle(triangle) should be (TriangleType.Scalene)
  }


}
