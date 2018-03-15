
import shapeless.{HList, ::, HNil}
import shapeless.ops.hlist.Last

trait Second[L <: HList] {
  type Out
  def apply(value: L): Out
}
object Second {
  type Aux[L <: HList, O] = Second[L] { type Out = O }
  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst
}
object DependentType {



  def main(args: Array[String]): Unit = {
    import Second._
    implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] = new Second[A :: B :: Rest] {
        type Out = B
        def apply(value: A :: B :: Rest): B =
          value.tail.head
      }


    val second1 = Second[Int :: String :: HNil]
    val second2 = Second[String :: Int :: HNil]

    println(second1)
    println(second2)
  }
}
