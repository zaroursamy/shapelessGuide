import shapeless.{Generic, HList, HNil}

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {

  def apply[A](implicit encoder: CsvEncoder[A]): CsvEncoder[A] = encoder

  def instance[T](f: T => List[String]) = {
    new CsvEncoder[T] {
      override def encode(value: T): List[String] = f(value)
    }
  }
}

object CsvWriter {
  def writeCsv[A](list: List[A], sep: String = ",")(implicit encoder: CsvEncoder[A]): String = list.map(e => encoder.encode(e).mkString(sep)).mkString("\n")

}

case class Employee(name: String, age: Int, manager: Boolean)

case class IceCream(name: String, numBoules: Int, isCornet: Boolean)

object Derivative {

  import CsvEncoder.instance

  implicit val employeeEncoder: CsvEncoder[Employee] = instance((e: Employee) => List(e.name, e.age.toString, if (e.manager) "yes" else "no"))
  implicit val iceCreamEncoder: CsvEncoder[IceCream] = instance((i: IceCream) => List(i.name, i.numBoules.toString, if (i.isCornet) "cornet" else "bol"))

  implicit def pairEncoder[A, B](implicit encoderA: CsvEncoder[A], encoderB: CsvEncoder[B]): CsvEncoder[(A, B)] = CsvEncoder.instance({ case (a, b) =>
    encoderA.encode(a) ++ encoderB.encode(b)
  }
  )


  def main(args: Array[String]): Unit = {

    val employees = List(Employee("toto", 24, true), Employee("tata", 19, false))
    val glaces = List(IceCream("italian fraise", 3, true), IceCream("chocolate", 1, false))

    import CsvWriter.writeCsv
    println(writeCsv(employees))
    println
    println(writeCsv(glaces))
    println()
    println(writeCsv(employees zip glaces))
  }

}

object Derivative2 {

  import CsvEncoder.instance

  implicit val stringEncoder = instance((s: String) => s :: Nil)
  implicit val intEncoder = instance((i: Int) => i.toString :: Nil)
  implicit val boolEncoder = instance((b: Boolean) => (if (b) "1" else "0") :: Nil)

  implicit val hnilEncoder = instance((_: HNil) => Nil)

  import shapeless.::

  implicit def hlistEncoder[H, T <: HList](implicit encoderH: CsvEncoder[H], encoderT: CsvEncoder[T]): CsvEncoder[H :: T] = instance(hlist =>
    encoderH.encode(hlist.head) ++ encoderT.encode(hlist.tail)
  )

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

  def main(args: Array[String]): Unit = {
    println(reprEncoder.encode("hello" :: 1 :: false :: HNil))

    import shapeless.Generic
    implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
      val gen = Generic[IceCream]
      val enc = CsvEncoder[gen.Repr]
      instance((i: IceCream) => enc.encode(gen.to(i)))
    }

    val glaces = List(IceCream("a", 1, true), IceCream("b", 2, false))
    println(CsvWriter.writeCsv(glaces))
  }

}

object Derivative3 {

  /* On définit tous les encoders */

  import CsvEncoder.instance

  implicit val stringEncoder = instance((s: String) => s :: Nil)
  implicit val intEncoder = instance((i: Int) => i.toString :: Nil)
  implicit val boolEncoder = instance((b: Boolean) => (if (b) "1" else "0") :: Nil)
  implicit val hnilEncoder = instance((_: HNil) => Nil)

  import shapeless.::

  implicit def hlistEncoder[H, T <: HList](implicit encoderH: CsvEncoder[H], encoderT: CsvEncoder[T]): CsvEncoder[H :: T] = instance(hlist =>
    encoderH.encode(hlist.head) ++ encoderT.encode(hlist.tail)
  )

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

  val glaces = List(IceCream("a", 1, true), IceCream("b", 2, false))

  implicit def genericEncoder[A, R](implicit generic: Generic.Aux[A, R] /* equivalent to generic:  Generic[A]{type Repr=R] */ ,
                                    repr: CsvEncoder[R]): CsvEncoder[A] = instance((a: A) => repr.encode(generic.to(a)))

  def main(args: Array[String]): Unit = {
    println(CsvWriter.writeCsv(glaces))
  }


}
