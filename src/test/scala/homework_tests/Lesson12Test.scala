package homework_tests

import org.scalatest.flatspec.AnyFlatSpec
import scala3_2.homework1.<+>
import scala3_2.homework1.~
import scala3_2.homework2.Completions
import scala3_2.homework3

class Lesson12Test extends AnyFlatSpec {

  "расширение конкатенации для String " should "должно конверировать операцией <+> в Int" in {
    assert("1" <+> "33" equals  133)

    assert("56" <+> "3" equals 563)

    assert("0000" <+> "005" equals 5)
  }

  it should "должно конверировать в примитивные типы" in {
    val res1:  Int = "0".~ + "2".~ + "1".~ + "33".~ + "45".~
    val res2:  Int = "0".~ - "2".~ - "1".~ - "33".~ - "45".~
    val res3:  Double = "0".~ + "2".~ + "1".~ + "33".~ + "45".~
    val res4:  String = "0".~ <> "2".~ <> "1".~ <> "33".~ <> "45".~
    val res5:  String = ("1".~ + "2".~) <> ("3".~ + "4".~) <> ("5".~ + "6".~)

    assert( res1 equals  213345)
    assert( res2 equals  4533120)
    assert( res3 equals  213345.0d)
    assert( res4 equals  "0 2 1 33 45")
    assert( res5 equals  "12 34 56")
  }

  "Completions" should "должен конвертировать примитивные типы в строковые сообщенния указывающие тип" in {
    assert(Completions.complete("String") === "String is String")
    assert(Completions.complete(1) === "1 is Int")
    assert(Completions.complete(7f) === "7.0 is Float" )
  }

  "Logarithm" should "должен возвращать ожидаемые значения" in {
    import homework3.Logarithm
    import Logarithm.*



    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)


    val l3 = l * l2
    val l4 = l + l2

    val s1 = Logarithm.safe(0)
    val s2 = Logarithm.safe(-2)
    val s3 = Logarithm.safe(2)

    assert(l === 0.0d)
    assert(l2 === 0.6931471805599453d)
    assert(l3  === 0.0d)
    assert(l4 === 0.6931471805599453d)

    assert(s1 === None)
    assert(s2 === None)
    assert(s3 === Some(0.6931471805599453d))

  }
}
