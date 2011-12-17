package org.scalaphilia.knp

import util.parsing.combinator.Parsers
import scalaz._
import Scalaz._
import util.parsing.input.CharSequenceReader


/**
 * Kanji number parser
 */
object KanjiNumParser extends Parsers {

  def main(args:Array[String]):Unit = {

    val number = "千五百兆二千二億三千五百十九万百一"
    nDigitNumbers(new CharSequenceReader(number)) match {
      case Success(ret, _) => println(ret.value)
      case _ => println("failed!")
    }
  }

  type Elem = Char




  sealed trait Num {
    def value: Long
  }

  case object EmptyNum extends Num {
    val value = 0L;
  }

  trait Digit extends Num {
    def e: Elem

    def value: Long = valueOf(e)
  }

  case class OneDigit(e: Elem) extends Digit

  case class MiddleDigit(e: Elem) extends Digit

  case class LargeDigit(e: Elem) extends Digit

  case class MiddleUnit(oneOp: Option[OneDigit], middle: MiddleDigit) extends Num {
    val value = oneOp.map( _.value * middle.value).getOrElse(middle.value)
  }

  case class MiddleGroup(thousandOp: Option[MiddleUnit],
                         hundredOp: Option[MiddleUnit],
                         tenOp: Option[MiddleUnit],
                         oneOp: Option[OneDigit]) extends Num {
    val value = thousandOp.value + hundredOp.value + tenOp.value + oneOp.value
  }

  case class LargeUnit(middleGroup: MiddleGroup, large: LargeDigit) extends Num {
    val value = middleGroup.value * large.value
  }

  case class NDigitNumbers(largeUnits: List[LargeUnit], middleGroupOp: Option[MiddleGroup]) extends Num {
    val value = (largeUnits.foldLeft(0L) {(total, num) => total + num.value}) + middleGroupOp.value
  }

  implicit def NumZero: Zero[Num] = zero(EmptyNum)

  implicit def numOp2Num(numOp: Option[Num]) = numOp match {
    case Some(num) => num
    case None => mzero[Num]
  }


  lazy val oneDigit = elem("oneDigit", isOneDigit(_)) ^^ (e => OneDigit(e))

  def middleDigitOf(e: Elem) = elem("middleDigit", _ == e) ^^ (e => MiddleDigit(e))

  lazy val largeDigit = elem("largeDigit", isLargeDigit(_)) ^^ (e => LargeDigit(e))

  def middleUnitOf(e: Elem) = oneDigit.? ~ middleDigitOf(e) ^^ {
    case (oneOp ~ middle) => MiddleUnit(oneOp, middle)
  }

  def middleGroup =
    middleUnitOf(elemOf(1000L)).? ~
      middleUnitOf(elemOf(100L)).? ~
      middleUnitOf(elemOf(10L)).? ~
      oneDigit.? ^? {
      case (th ~ hu ~ te ~ one) if List(th, hu, te, one).flatten.nonEmpty =>
        MiddleGroup(th, hu, te, one)
    }


  def largeUnit = middleGroup ~ largeDigit ^^ {
    case (mGroup ~ large) => LargeUnit(mGroup, large)
  }

  def nDigitNumbers = largeUnit.* ~ middleGroup.? ^? {
    case (lUnits ~ mGroupOp) if !(lUnits.isEmpty && mGroupOp.isEmpty) =>
      NDigitNumbers(lUnits, mGroupOp)
  }

  private def isOneDigit(e: Elem): Boolean = numberMap.get(e) match {
    case Some(n) if 0L <= n && n <= 9L => true
    case _ => false
  }

  private def isMiddleDigit(e: Elem): Boolean = numberMap.get(e) match {
    case Some(n) if 10L <= n && n <= 1000L => true
    case _ => false
  }

  private def isLargeDigit(e: Elem): Boolean = numberMap.get(e) match {
    case Some(n) if 1000L <= n => true
    case _ => false
  }

  private def valueOf(e: Elem): Long = numberMap(e)

  private def elemOf(n: Long): Elem = numberMap.find(_._2 == n).map(_._1).get

  val numberMap: Map[Char, Long] = Map(
    '一' -> 1L,
    '二' -> 2L,
    '三' -> 3L,
    '四' -> 4L,
    '五' -> 5L,
    '六' -> 6L,
    '七' -> 7L,
    '八' -> 8L,
    '九' -> 9L,
    '十' -> 10L,
    '百' -> 100L,
    '千' -> 1000L,
    '万' -> 10000L,
    '億' -> 100000000L,
    '兆' -> 1000000000000L,
    '京' -> 10000000000000000L
  )

}