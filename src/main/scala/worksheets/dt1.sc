

case class Vec(x: Double, y: Double)

case class Rec(origin: Vec, size: Vec)

import shapeless.Generic

def getRepr[A](a: A)(implicit gen: Generic[A]) = Generic[A].to(a)

getRepr(Vec(0, 0))
getRepr(Rec(Vec(0, 0), Vec(1, 1)))

/********/

import shapeless.{HList, ::, HNil}
import shapeless.ops.hlist.Last

val last1 = Last[Int :: String :: HNil]
val last2 = Last[String :: Int :: HNil]

last1(1 :: "last1" :: HNil)
last2("last2" :: 2 :: HNil)