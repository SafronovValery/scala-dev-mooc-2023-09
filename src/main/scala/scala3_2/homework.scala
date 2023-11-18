package scala3_2

import scala3_2.homework1.ExtString

import scala.annotation.targetName
import scala.language.implicitConversions



object  homework1 :



  /////1. операция + для строки в отличии от < не перегружается, используем операцию <+>
  extension (x: String)
    def <+>(y: String): Int = (x + y).toInt
    //def +(y: String): Int = (x + y).toInt // + не перегружается


  /////2. конвертируем строки в тип ExtString и на нем определяем операцию конкатенации +, -, <>
  // В отличии от реализации 1 допустимы цепочки операций и скобки

  case class ExtString(value: String) extends AnyVal:
    def + (ey: ExtString): ExtString = ExtString(value + ey.value)
    def - (ey: ExtString): ExtString = ExtString(ey.value + value) //конкатенация в обратном порядке
    def <> (ey: ExtString): ExtString = ExtString(value + " " + ey.value) //конкатенация со вставкой пробела

  implicit def exToInt(ex: ExtString):Int = ex.value.toInt
  implicit def exToString(ex: ExtString):String = ex.value
  implicit def exToDouble(ex: ExtString):Double = ex.value.toDouble

  extension (x: String) def ~ : ExtString = ExtString(x)


  @main def part1Ex(): Unit ={
      val sm1: Int = "1" <+> "33"
      val sm2 = "56" <+> "3"
      //val smerr:Int = "56" <+> "3" <+> "7"  //не компилируется - только два аргумента :(


      println(sm1)
      println(sm2)


      val sm3:Int = "0".~ + "2".~ + "1".~ + "33".~ + "45".~
      val sm4:Int = "0".~ - "2".~ - "1".~ - "33".~ - "45".~
      val sm5:String = "0".~ <> "2".~ <> "1".~ <> "33".~ <> "45".~
      val sm6:Double = "0".~ + "2".~ + "1".~ + "33".~ + "45".~
      val sm7:String = ("1".~ + "2".~) <> ("3".~ + "4".~) <> ("5".~ + "6".~)

      //допустимы цепочки операций :) но тип перменной должен быть обявлен
      // для имплицитного преобразования типа ExtString в примитивный тип
      println(sm3)
      println(sm4)
      println(sm5)
      println(sm6)
      println(sm7)
  }



object homework2 {

  enum CompletionArg :
    case ShowItIsString(s: String)
    case ShowItIsInt(i: Int)
    case ShowItIsFloat(f: Float)
    //см приведенную ссылку


  object CompletionArg :
    given fromString: Conversion[String, CompletionArg] = ShowItIsString(_)
    given fromInt: Conversion[Int, CompletionArg] = ShowItIsInt(_)
    given fromFloat: Conversion[Float, CompletionArg] = ShowItIsFloat(_)


  object Completions :

    import CompletionArg.*

    def complete[T](arg: CompletionArg): String = arg match
      case ShowItIsString(x) => s"$x is String"
      case ShowItIsInt(x) => s"$x is Int"
      case ShowItIsFloat(x) => s"$x is Float"

    @main def part2Ex(): Unit =
      println(Completions.complete("String"))
      println(Completions.complete(1))
      println(Completions.complete(7f))

}


object homework3 {
  opaque type Logarithm = Double

  object Logarithm:
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if d > 0.0 then Some(math.log(d)) else None


  extension (x: Logarithm) {
    def toDouble: Double = math.exp(x)
    def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def *(y: Logarithm): Logarithm = x + y
  }

  @main def part3Ex(): Unit =
    import homework3.Logarithm
    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2
    val l0 = Logarithm(0)
    val s1 = Logarithm.safe(0)
    val s2 = Logarithm.safe(-2)
    val s3 = Logarithm.safe(2)
    println(s"$l    $l2    $l3    $l4   $l0")
    println(s"$s1    $s2    $s3")


}
