import module2.higher_kinded_types.{tuplef, tuplef_2, tuplef_3}

object Main {

  def main(args: Array[String]): Unit = {

    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5)

    println("tuplef")
    println(tuplef(optA, optB))
    println(tuplef(list1, list2))
    println

    println("tuplef_2")
    println(tuplef_2(optA, optB))
    println(tuplef_2(list1, list2))
    println

    println("tuplef_3")
    println(tuplef_3(optA, optB))
    println(tuplef_3(list1, list2))
    println(tuplef_3(list2, list1))
    println(tuplef_3(list1, List()))
    println(tuplef_3(List(), list2))

}
}