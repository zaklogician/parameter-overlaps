package overlaps

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.Gen.{choose,listOf, alphaStr, numChar}
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary


object LinProps extends Properties("Lin") {

  import overlaps.Main.Lin

  val lins = for {
    a <- Gen.choose(0, 10000)
    b <- Gen.choose(-100, 10000)
  } yield Lin(a,b)
  implicit val arbLin = Arbitrary(lins)

  property("toString sensible") = Prop.forAll { (a: Int, b: Int) =>
    val aL = Lin(a, 0)
    val abL = Lin(a,b)
    a == 0 || abL.toString.startsWith(aL.toString)
  }

  property("\b(n,0) dominates constants") = Prop.forAll { (a: Int, b: Int, c: Int) =>
    val lin = Lin(a,b)
    val con = Lin(0,c)
    a <= 0 || (lin dominates con)
  }

  property("mul > that.mul dominates") = Prop.forAll { (aL: Lin, bL: Lin) =>
    aL.mul <= bL.mul || (aL dominates bL)
  }

  property("mul == that.mul them larger _.add dominates") = Prop.forAll { (a: Int, b: Int, c: Int) =>
    val aL = Lin(a,b)
    val bL = Lin(a,c)
    b <= c || (aL dominates bL)
  }

  property("!dominates then dominatesFrom == None") = Prop.forAll { (aL: Lin, bL: Lin) =>
    (aL dominates bL) || ((aL dominatesFrom bL) == None)
  }

  property("dominatesFrom actually dominates") = Prop.forAll {
    (aL: Lin, bL: Lin, n: Int) =>
    (n < -10000 || n > 10000) || (aL dominatesFrom bL match {
      case None => true
      case Some(k: Int) => (n <  k && aL.mul*n + aL.add <  bL.mul*n + bL.add) ||
                           (n >= k && aL.mul*n + aL.add >= bL.mul*n + bL.add)
    })
  }

  property("+ commutative") = Prop.forAll {
    (aL: Lin, bL: Lin) => aL + bL == bL + aL
  }

  property("+ associative") = Prop.forAll {
    (aL: Lin, bL: Lin, cL: Lin) => (aL + bL) + cL == aL + (bL + cL)
  }

  property("-- inverts") = Prop.forAll {
    (aL: Lin) => (aL -- aL).isZero
  }

  property("- is -- if dominates") = Prop.forAll {
    (aL: Lin, bL: Lin) => !(aL dominates bL) || (aL - bL) == (aL -- bL)
  }

  property("overlap contains zero") = Prop.forAll {
    (aL: Lin, bL: Lin) => {
      val abL = aL overlap bL
      abL._1.isZero || abL._2.isZero || abL._3.isZero
    }
  }

  property("overlap symmetric") = Prop.forAll {
    (aL: Lin, bL: Lin) =>
      val abL = aL overlap bL
      val baL = bL overlap aL
      (abL._1 == baL._3) && (abL._2 == baL._2) && (abL._3 == baL._1)
  }

  property("overlap contains this or that") = Prop.forAll {
    (aL: Lin, bL: Lin) =>
      val abL = aL overlap bL
      val abs = List(aL,bL)
      abs.contains( abL._1 ) || abs.contains( abL._2 ) || abs.contains( abL._3 )
  }

}

object CharExpProps extends Properties("CharExp") {

  import overlaps.Main.Lin
  import overlaps.Main.CharExp

  val charExps = for {
    a <- Gen.choose(0, 10000)
    b <- if (a == 0) Gen.choose(1,10000) else Gen.choose(-100, 10000)
    c <- Gen.oneOf("abcdefghijklmnopqrstuvwxyzXYZ")
  } yield CharExp(c, Lin(a,b))
  implicit val arbcharExp = Arbitrary(charExps)

  property("overlap outputs matching character") = Prop.forAll {
    (aC: CharExp, bC: CharExp) =>
    aC overlap bC match {
      case (_,Some(rC),_) => rC.char == aC.char && rC.char == bC.char
      case (_,None,    _) => aC.char != bC.char
    }
  }

}

object Util extends Properties("Util") {
  import overlaps.Main.Lin
  import overlaps.Main.CharExp
  import overlaps.Main.Util._
 
  implicit val arbcharExp = Arbitrary(overlaps.CharExpProps.charExps)
  

  property("overlap invariant with replace") = Prop.forAll {
    (a: CharExp, b: CharExp, c: CharExp, d: CharExp, e: CharExp, f: CharExp) =>
    val ns = List(a,b,c)
    val ms = List(d,e,f)
    val fc = CharExp('*', Lin(1,0))
    def replace(xs: List[CharExp]): List[CharExp] = ns match {
      case Nil => xs
      case (y :: ys) => xs.map( c => if (c == y) fc else c)
    }
    replace( ovlap(ns,ms) ) == ovlap( replace(ns), replace(ms) )
  }

  property("overlap quasi-invariant with translation") = Prop.forAll {
    (xs: List[CharExp], ys: List[CharExp], zs: List[CharExp]) =>
    val ov = ovlap(xs,ys)
    ov == xs || ov == ys || ov == ovlap(zs ++ xs, ys ++ zs)
  }

  property("overlap test cases") = Prop {
    val x  = CharExp('x', Lin(0,1))
    val y  = CharExp('y', Lin(0,1))
    val x2 = CharExp('x', Lin(0,2))
    val y2 = CharExp('y', Lin(0,2))
    val xn = CharExp('x', Lin(1,0))
    val yn = CharExp('y', Lin(1,0))

    ovlap( List(x2,y) , List(x,y,xn) ) == List(x,y)    &&
    ovlap( List(x2,y) , List(x2) ) == List()           &&
    true
  }

}
