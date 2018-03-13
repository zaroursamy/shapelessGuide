case class Samy(name:String, age:Int)
case class Samia(name:String, age:Int)

import shapeless._
val hSamy = Generic[Samy].to(Samy("samy",25))
val hSamia = Generic[Samia].to(Samia("samia",23))

hSamia.head
hSamia.tail
hSamia.last

hSamy.tail
hSamy.tail.tail

val hSamySamia = Samy("samy",25) :: hSamia

case class IceCream(name:String, numCherries: Int, isCone: Boolean)

val iceCream = IceCream("italian", 2, true)

val iceCreamGen = Generic[IceCream]


val repr = iceCreamGen.to(iceCream)

val iceCream2 = iceCreamGen.from(repr)

Generic[Samy].from(Generic[Samia].to(Samia("samia",23)))


case class Red()
case class Green()
case class Blue()
type Light = Red :+: Green :+: Blue :+: CNil

val red: Light = Inl(Red())


