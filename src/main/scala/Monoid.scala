trait Monoid[A]{
  def id: A
  def compose(a:A, b:A): A
}

case class Visite(debut: Long, fin:Long, id:String)

object Monoid {

  def instance[A](a:A, g: ((A,A)=>A)) = new Monoid[A] {
    override def id: A = a

    override def compose(a: A, b: A): A = g(a,b)
  }

  implicit val stringMonoid: Monoid[String] = instance("", (a: String,b:String) => a+b)

  implicit val intMonoid: Monoid[Int] = instance(0, (a:Int, b:Int) => a+b)
  implicit val visiteMonoid: Monoid[Visite] = instance(Visite(0,0,""), (v1, v2) => Visite(math.min(v1.debut, v2.debut), math.max(v1.fin, v2.fin), v1.id))

  implicit def monoidOpt[A](implicit m: Monoid[A]): Monoid[Option[A]] = instance(None, (aOpt:Option[A], bOpt: Option[A]) => (aOpt: Option[A],bOpt: Option[A]) match {
    case (Some(a), Some(b)) => Option(m.compose(a,b))
    case (None, None) => None
    case (Some(_), _) => aOpt
    case (_, Some(_)) => bOpt
  })

//  implicit def monoidOptOpt[A](implicit m:Monoid[Option[A]]) = instance(None, (a:Option[Option[A]], b:Option[Option[A]]) => {
//
//    (a,b) match {
//      case (Some(x), Some(y)) => m.compose(x,y)
//    }
//    Option(m.compose(a.flatten, b.flatten))
//  })


//  implicit def monade[A](f:A=>A, g:A=>A): Monoid[A=>A] = instance(identity, (ff, gg) => ff.andThen(gg))

}

object Main2{

  import Monoid._

  def sum[A](a:A, b:A)(implicit m: Monoid[A]) = m.compose(a,b)
  def main(args: Array[String]): Unit = {



//    println(sum(Option("1"), Option("2")))
//    println(sum(Option(1), None))
//    println(sum(1, 2))
//    println(sum(1.toString, 2.toString))
    println(sum(Option(Option(1)), Option(Option(8))))

//    println(sum(Visite(1,4, "A"), Visite(5,6,"A")))

  }
}