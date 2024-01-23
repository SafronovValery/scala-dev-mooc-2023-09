package module3


import module3.zioConcurrency.printEffectRunningTime
import module3.zio_homework._
import module3.zio_homework.printRunningTime.PrintRunningTime
import zio.clock.Clock
import zio.config._
import zio.config.magnolia._
import zio.config.typesafe.TypesafeConfigSource
import zio.console._
import zio.duration.durationInt
import zio.random._
import zio.{Has, RIO, Task, UIO, ULayer, URIO, ZIO, ZLayer, clock}

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.{BufferedSource, Source}
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  def put(str: String): URIO[Console, Unit] = putStrLn(str)

  val readNumber: URIO[Console, Int] = getStrLn.flatMap(str => ZIO.effect(str.toInt)).orElse {
    put("Некорректный ввод, попробуйте снова").zipRight(readNumber)
  }

  val readGoogNumber:URIO[Console, Int] = readNumber.flatMap{
    nmb => if (nmb >= 1 && nmb <= 3) ZIO.succeed(nmb)
    else put("Число должно быть от 1 до 3. Повторите ввод").zipRight(readGoogNumber)
  }

  val random: URIO[Random, Int] = nextIntBetween(1,3)

  val guessProgram: URIO[Console with Random,  Unit] =  for {
    rnd <- random
    _ <- put("Угадайте число от 1 до 3 (введите в консоли)")
    nmb <- readGoogNumber
    _ <- put(s"Случайное число $rnd Вы ${if (rnd == nmb) "угадали" else "не угадали"}")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhil[R,E,A](effect: ZIO[R,E,A])(predicat: A => Boolean ): ZIO[R,E,A] = effect.flatMap{
    a => if (predicat(a)) effect else  doWhil(effect)(predicat)
  }


  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  val path = "src/test/scala/home/test.conf"

  def openFile(fileName: String): Task[BufferedSource] = ZIO.effect(Source.fromFile(fileName))

  def closeFile(file: Source): UIO[Unit] = ZIO.effect(file.close()).orDie

  def handleFileTest(file: Source): URIO[Console, List[Unit]] = ZIO.foreach(file.getLines().toList) { str =>
    putStrLn(str)
  }

  final case class MailConfig(host: String, port: Int, ssl: Boolean = true, tls: Boolean = true, user: String, password: String,
                        fromName: String, subjectPrefix: String, subjectSuffix: String, to: String, toName: String, moreRecipients: Boolean = false)

  val defaultMailConfig = MailConfig("smtp.mail.ru", 465, true, true, "test.test@mail.ru", "abracadabra",
    "Сеть ресторанов", "Заказ №", "", "safronoff2024@gmail.com", "Валерий Сафронов", false)


  implicit class ImpureEither[A, B](either: Either[A, B]) {
    def loadOrThrow: B = either match {
      case Left(e) => throw new Exception(e.toString)
      case Right(v) => v
    }
  }



  def handleFile(file: Source): Task[MailConfig] = ZIO.effect{
    val strb = new StringBuilder
    val myConfig = descriptor[MailConfig]
    file.getLines().foreach{str =>
      strb.append(str)
      strb.append("\n")
    }
    val hoconStr = strb.toString
    val hoconSource = TypesafeConfigSource.fromHoconString(hoconStr).loadOrThrow
    val re: Either[ReadError[String], MailConfig] = read(myConfig from hoconSource)
    re match {
      case Left(err: ReadError[String]) => throw new Exception(err)
      case Right(mailConfig: MailConfig) =>  mailConfig
    }
  }.tapError {
    e => ZIO.effect(println(e)).orDie
  }.orElse(ZIO.succeed(defaultMailConfig))


  def loadConfigOrDefault: RIO[Console,  MailConfig] = for {
    config <- ZIO.bracket(openFile(path))(closeFile){ file => handleFile(file) }
    _ <- putStrLn(config.toString)

  } yield config

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */

  val eff: RIO[Random with Clock, Int] = ZIO.sleep(1 seconds) zipRight nextIntBetween(0,10)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  val effects: List[RIO[Random with Clock, Int]] =  (1 to 10).toList.map{
    _ => eff
  }

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */


  val effectCollect :  RIO[Random with Clock, List[Int]] =  ZIO.collectAll(effects)
  val effectSum: RIO[Random with Clock, Int] = effectCollect.flatMap{
    list =>
      val sum = list.foldLeft(0){ (acc, rnd ) => acc + rnd}
      ZIO.succeed(sum)
  }
  val appBody =   for {
    _ <- putStrLn("begin run")
    sum <- effectSum
    _ <- putStrLn(sum.toString)
  } yield ()

  val app = printEffectRunningTime(appBody)


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  val effectCollectSpeedUp :  RIO[Random with Clock, List[Int]] =  ZIO.collectAllPar(effects)
  val effectSumSpeedUp: RIO[Random with Clock, Int] = effectCollectSpeedUp.flatMap {
    list =>
      val sum = list.foldLeft(0) { (acc, rnd) => acc + rnd }
      ZIO.succeed(sum)
  }
  val appBodySpeedUp = for {
    _ <- putStrLn("begin run")
    sum <- effectSumSpeedUp
    _ <- putStrLn(sum.toString)
  } yield ()

  val appSpeedUp = printEffectRunningTime(appBodySpeedUp)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */
object printRunningTime {
    type PrintRunningTime = Has[PrintRunningTime.Service]


    object  PrintRunningTime {

      trait Service {
        def printTime[R, E, A](effect: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A]
      }

      class ServiceImpl extends Service {
        val currentTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.SECONDS)

        override def printTime[R, E, A](effect: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] = for {
          start <- currentTime
          r <- effect
          end <- currentTime
          _ <- putStrLn(s"Running time ${end - start}")
        } yield r
      }


      val live: ULayer[PrintRunningTime] = ZLayer.succeed(new ServiceImpl)

      def printTime[R,E,A](effect: ZIO[R, E, A]):  ZIO[PrintRunningTime with R with Clock with Console,  E, A] = ZIO.accessM(_.get.printTime(effect))
    }
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */


    //создание и использование сервиса  ниже в коде в объекте buildingAndRunZIOServices


  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  val runApp = buildingAndRunZIOServices.createEffectForRun

}

object buildingAndRunZIOServices extends App {
  def createEffectForRun: ZIO[Console with Random with Clock, Throwable, Unit] = {

    def injectEffect[R, E, A](eff: ZIO[R, E, A]): ZIO[PrintRunningTime with R with Clock with Console, E, A] = PrintRunningTime.printTime(eff)

    val appWithTimeLogg: ZIO[PrintRunningTime with Console with Random with Clock, Throwable, Unit] = injectEffect(appBody)

    val env: ULayer[PrintRunningTime] = PrintRunningTime.live

    val effectForRun: ZIO[Console with Random with Clock, Throwable, Unit] = appWithTimeLogg.provideSomeLayer[Console with Random with Clock](env)

    effectForRun
  }



  zio.Runtime.default.unsafeRun(createEffectForRun)



}

object TestRun extends App {

  zio.Runtime.default.unsafeRun(guessProgram)


  val e1: ZIO[Console, IOException, String] = for {
    str <- getStrLn
    _ <- putStrLn(str)
  } yield  str


  val ef: ZIO[Console, IOException, String] = doWhil( e1 )(_ == "quit")
  zio.Runtime.default.unsafeRun(ef)


 val r: MailConfig = zio.Runtime.default.unsafeRun(loadConfigOrDefault)



  val testEff = for {
    _ <- putStrLn("begin")
    rnd <- eff
    _ <- putStr(rnd.toString)
  } yield  ()

  zio.Runtime.default.unsafeRun(testEff)



  val testEffectSum = for {
    _ <- putStrLn("begin")
    sum <- effectSum
    _ <- putStrLn(sum.toString)
  } yield ()


  zio.Runtime.default.unsafeRun(app)

  zio.Runtime.default.unsafeRun(appSpeedUp)

}

