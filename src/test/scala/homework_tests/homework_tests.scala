package homework_tests

import module2.higher_kinded_types.{tuplef, tuplef_2, tuplef_3, tuplef_4}
import org.scalatest.flatspec.AnyFlatSpec

class homework_tests extends AnyFlatSpec {
  "tuplef" should "должен соответствовать ожидаемому поведению" in {
    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)
    val optC: Option[Int] = None

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5)

    //Option
    assert(tuplef(optA, optB) === Some((1,2)))
    assert(tuplef(optA, optA) === Some((1,1)))
    assert(tuplef(optB, optB) === Some((2,2)))
    assert(tuplef(optB, optA) === Some((2,1)))
    assert(tuplef(optA, optC) === None)
    assert(tuplef(optC, optB) === None)

    //List
    assert(tuplef(list1, list2) === List((1,4), (1,5), (2,4), (2,5), (3,4), (3,5)))
    assert(tuplef(list2, list1) === List((4,1), (4,2), (4,3), (5,1), (5,2), (5,3)))
    assert(tuplef(list1, List()) === Nil)
    assert(tuplef(List(), list2) === Nil)
  }

  "tuplef_2" should "должен соответствовать ожидаемому поведению" in {
    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)
    val optC: Option[Int] = None

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5)

    //Option
    assert(tuplef_2(optA, optB) === Some((1, 2)))
    assert(tuplef_2(optA, optA) === Some((1, 1)))
    assert(tuplef_2(optB, optB) === Some((2, 2)))
    assert(tuplef_2(optB, optA) === Some((2, 1)))
    assert(tuplef_2(optA, optC) === None)
    assert(tuplef_2(optC, optB) === None)

    //List
    assert(tuplef_2(list1, list2) === List((1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5)))
    assert(tuplef_2(list2, list1) === List((4, 1), (4, 2), (4, 3), (5, 1), (5, 2), (5, 3)))
    assert(tuplef_2(list1, List()) === Nil)
    assert(tuplef_2(List(), list2) === Nil)
  }

  "tuplef_3" should "должен соответствовать ожидаемому поведению" in {
    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)
    val optC: Option[Int] = None

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5)

    //Option
    assert(tuplef_3(optA, optB) === Some((1, 2)))
    assert(tuplef_3(optA, optA) === Some((1, 1)))
    assert(tuplef_3(optB, optB) === Some((2, 2)))
    assert(tuplef_3(optB, optA) === Some((2, 1)))
    assert(tuplef_3(optA, optC) === None)
    assert(tuplef_3(optC, optB) === None)

    //List
    assert(tuplef_3(list1, list2) === List((1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5)))
    assert(tuplef_3(list2, list1) === List((4, 1), (4, 2), (4, 3), (5, 1), (5, 2), (5, 3)))
    assert(tuplef_3(list1, List()) === Nil)
    assert(tuplef_3(List(), list2) === Nil)
  }

  "tuplef_4" should "должен соответствовать ожидаемому поведению" in {
    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)
    val optC: Option[Int] = None

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5)

    //Option
    assert(tuplef_4(optA, optB) === Some((1, 2)))
    assert(tuplef_4(optA, optA) === Some((1, 1)))
    assert(tuplef_4(optB, optB) === Some((2, 2)))
    assert(tuplef_4(optB, optA) === Some((2, 1)))
    assert(tuplef_4(optA, optC) === None)
    assert(tuplef_4(optC, optB) === None)

    //List
    assert(tuplef_4(list1, list2) === List((1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5)))
    assert(tuplef_4(list2, list1) === List((4, 1), (4, 2), (4, 3), (5, 1), (5, 2), (5, 3)))
    assert(tuplef_4(list1, List()) === Nil)
    assert(tuplef_4(List(), list2) === Nil)
  }
}
