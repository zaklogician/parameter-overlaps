package overlaps


object Main {

  case class Lin(mul: Int, add: Int) {
    override def toString: String = mul match {
      case 0 => add match {
        case 0 => "0"
        case _ => "" + add
       }
      case 1 => add match {
        case 0 => "n"
        case _ if (add < 0) => "n - " + math.abs(add)
        case _              => "n + " + add
      }
      case _ => add match {
        case 0 => mul + "n"
        case _ if (add < 0) => mul + "n - " + math.abs(add)
        case _              => mul + "n + " + add
      }
    }

    def dominates(that: Lin): Boolean = {
      (this.mul > that.mul) || (this.mul == that.mul && this.add > that.add)
    }

    def isZero: Boolean = {
      this.mul == 0 && this.add == 0
    }

    def dominatesFrom(that: Lin): Option[Int] = {
      if (this dominates that) {
         val k = (that.add - this.add).toDouble / (this.mul - that.mul).toDouble
         val K: Int = math.ceil( k ).toInt
         Some(K)
      } else None
    }

    def overlap(that: Lin): (Lin,Lin,Lin) = {
      val left = if (this dominates that)
        this -- that else Lin(0,0)
      val mid = if (this dominates that) that else this
      val right = if (this dominates that)
        Lin(0,0) else that -- this
      (left,mid,right)
    }

    def +(that: Lin): Lin = Lin(this.mul + that.mul, this.add + that.add)
    def -(that: Lin): Lin = Lin(this.mul - that.mul, this.add - that.add)
    def --(that: Lin): Lin = {
      if (that dominates this) Lin(0,0) else this - that
    }
  }


  /* sealed trait IsCharExp {
    val exp: Lin

    override def equals(that: Any): Boolean = (this,that) match {
      case (Null, Null) => true
      case (Null, CharExp(_,Lin(0,0))) => true
      case (CharExp(_,Lin(0,0)),Null) => true
      case (CharExp(c1,e1),CharExp(c2,e2)) => c1 == c2 && e1 == e2
      case _ => false
    }
  }
  case object Null extends IsCharExp { val exp = Lin(0,0) } */
  case class CharExp(char: Char, exp: Lin) {
    require( exp.mul >= 0 && (exp.mul != 0 || exp.add >= 0) )
    override def toString: String = char + "^(" + exp + ")"

    def overlap(that: CharExp): (CharExp, Option[CharExp], CharExp) = {
      if (this.char == that.char) {
        val coeffs = this.exp overlap that.exp
        ( CharExp(this.char, coeffs._1)
        , Some( CharExp(this.char, coeffs._2) )
        , CharExp(this.char, coeffs._3) )
      } else (this, None, that)
    }

  }
  
  case class StringExp(toList: List[CharExp]) {
    override def toString: String = toList mkString ""

    def overlap(that: StringExp): (StringExp, StringExp, StringExp) = {
      ???
    }
  }

  object Util {


    def ovlap(lh:List[CharExp], rh: List[CharExp]) = overlap(lh,rh,List(),List())

    def overlap(
      lhs: List[CharExp],
      rhs: List[CharExp],
      resAcc: List[CharExp],
      infoAcc: List[(CharExp,CharExp)]
    ): List[CharExp] = (lhs,rhs) match {
       case (l :: ls, r :: rs) => {
         (l overlap r) match {
            case (_,Some(m),_) => overlap(ls, rs, resAcc :+ m, infoAcc :+ (l,r))
            case _             => overlap(
              (infoAcc.map(_._1) ++ lhs).tail,
              infoAcc.map(_._2) ++ rhs,
              List(), List()
            )
         }
       }
       case (Nil, _) => resAcc
       case (_, Nil) => overlap(
              infoAcc.map(_._1).tail ++ lhs,
              infoAcc.map(_._2) ++ rhs,
              List(), List()
            )
    }


  }
  
  
  
  def main(args: Array[String]): Unit = {
    println("Hello World")
  }
}
