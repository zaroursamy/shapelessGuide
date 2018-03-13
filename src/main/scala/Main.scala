import shapeless.Generic
sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape
object Main {


  def main(args: Array[String]): Unit = {

    val gen = Generic[Shape]

    println(gen.to(Rectangle(1,1)))
    println(gen.to(Circle(1)))
  }
}