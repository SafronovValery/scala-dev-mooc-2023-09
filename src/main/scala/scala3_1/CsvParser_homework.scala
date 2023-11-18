package scala3_1





//1. исполользовать given, как написано в комментариях и в почеченных местах ниже
//2. использовать новый "тихий синтаксис", где сочтете приемлемым, тут на ваше усмотрение
//https://docs.scala-lang.org/scala3/new-in-scala3.html  глава New & Shiny: The Syntax
//главное это разобраться с given


class MonadParser[T, Src](private val p: Src => (T, Src)) {
  def flatMap[M](f: T => MonadParser[M, Src]): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      val mn = f(word)
      val res = mn.p(rest)

      //с помощью функции — аргумента метода добавляем его в контекст, видимый всем последующим парсерам по цепочке.
      res
    }

  def map[M](f: T => M): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      (f(word), rest)
    }

  def parse(src: Src): T = p(src)._1
}

object MonadParser

def apply[T, Src](f: Src => (T, Src)) = new MonadParser[T, Src](f)


trait FieldConversion[A, B]:
  def convert(x: A): B

given intFieldConversion: FieldConversion[String, Int] with
  def convert(x: String): Int = x.toInt

given floatFieldConversion: FieldConversion[String, Float] with
  def convert(x: String): Float = x.toFloat

given doubleFieldConversion: FieldConversion[String, Double] with
  def convert(x: String): Double = x.toDouble

given booleanFieldConversion: FieldConversion[String, Boolean] with
  def convert(x: String): Boolean = x.toBoolean

// сделать given instance для типов Int Float Double
// в функции просто сконвертнуть строку в нужный тип


///


object TestExecution {

  //здесь написать функцию, которая будет применять given определенные выше
  // использовать using fieldConversion c первым параметром String, а второй будет вариативны параметр B

  def parse[B](x: String)(using convertor: FieldConversion[String, B]): B = convertor.convert(x)
  //    ...вызвать собственнь функцию из трейта FieldConversion...

  private def StringField: MonadParser[String, String] =
    MonadParser[String, String] { str =>
      val idx = str.indexOf(";")
      if (idx > -1)
        (str.substring(0, idx), str.substring(idx + 1))
      else
        (str, "")
    }


  private def IntField: MonadParser[Int, String] = StringField.map(parse[Int])

  private def BooleanField: MonadParser[Boolean, String] = StringField.map(parse[Boolean])

  private def FloatField: MonadParser[Float, String] = StringField.map(parse[Float])

  private def DoubleField: MonadParser[Double, String] = StringField.map(parse[Double])
  //StringField.map(...здесь применить parse который подхватит нужный given автоматически ...)


  case class Car(year: Int, mark: String, model: String, comment: String, price: Float)

  val parser: MonadParser[Car, String] =
    for {
      year <- IntField
      mark <- StringField
      model <- StringField
      comment <- StringField
      price <- FloatField
    } yield Car(year, mark, model, comment, price)

  trait TextSplit[T]:
    def splitText(x: T): Array[T]

  private def getOperatingSystem(): String = System.getProperty("os.name").toLowerCase

  given stringTextSplit: TextSplit[String] with {
    def splitText(x: String): Array[String] =
      val os = getOperatingSystem()
      val osType = if os.contains("windows")
      then "win" else if os.contains("linux") then "linux"
      else if os.contains("mac") then "mac"
      else ""

      val splitter = osType match
        case "win" => "\r\n"
        case "linux" => "\n"
        case "mac" => "\r"
        case _ => "\n"

      x.split(splitter)
  }

  def split[T](x: T)(using cpl: TextSplit[T]): Array[T] = cpl.splitText(x)

  extension (s: String)
    def splitText: Array[String] = split(s)


}

import TestExecution._

@main def main(): Unit = {


  val str = "1997;Ford;E350;ac, abs, moon;3000\n1996; Jeep; Grand Cherokee; MUST SELL! air, moon roof, loaded; 4799"

  val result: Array[Car] = split(str).map(parser.parse)
  val result2: Array[Car] = str.splitText.map(parser.parse)

  result foreach println
  println("-------------")
  result2 foreach println


}
