import module1.threads.{Foo, Thread1, ToyFuture, getRatesLocation1, getRatesLocation2, getRatesLocation3, getRatesLocation4, getRatesLocation5, getRatesLocation6, getRatesLocation7, getRatesLocation8, printRunningTime}
import module1.{future, promise, try_}
import module2.implicits
import module3.functional_effects.functionalProgram
import module3.functional_effects.functionalProgram.{declarativeEncoding, executableEncoding}

import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object Main {

  def main(args: Array[String]): Unit = {
    println(s"Hello world " +
      s"[${Thread.currentThread().getName}]")

//    val t1 = new Thread{
//      override def run(): Unit = {
//        Thread.sleep(1000)
//        println(s"Hello from " +
//          s"[${Thread.currentThread().getName}]")
//      }
//    }
//    val t2 = new Thread1
//    t1.start()
//    t2.start()
//    t1.join()

//    def rates = {
//      val start = System.currentTimeMillis()
//
//      getRatesLocation7.onComplete{ i1 =>
//        getRatesLocation8.onComplete{ i2 =>
//          println(i1 + i2)
//          val end = System.currentTimeMillis()
//          println(s"Execution time ${end - start}")
//        }
//      }
//    }
//
//    val result: ToyFuture[Int] = for{
//      i1 <- getRatesLocation7
//      i2 <- getRatesLocation8
//    } yield i1 + i2
//
//
//    printRunningTime(rates)

//      val f: Future[(String, String)] = future.printRunningTime(future.getRatesLocation1
//        .flatMap(r =>
//          future.getRatesLocation2.map(r2 =>
//            (r, r2))(scala.concurrent.ExecutionContext.global))
//      (scala.concurrent.ExecutionContext.global))

//    f.onComplete {
//      case Failure(exception) => println(exception.getMessage)
//      case Success(value) => println(value)
//    }(scala.concurrent.ExecutionContext.global)

//    val r = Await.result(future.f9, 3.second)
//    println(r)

//    println(promise.p1.isCompleted)
//    println(promise.f1.isCompleted)
//    promise.p1.failure(new Exception("ooops"))
//    println(promise.p1.isCompleted)
//    println(promise.f1.isCompleted)
//    println(Await.result(promise.f1.recover{case _ => 0}(scala.concurrent.ExecutionContext.global), 3.second))

    val p: executableEncoding.Console[Unit] = functionalProgram.executableEncoding.greet
    val p2: executableEncoding.Console[Unit] = p.flatMap(_ => functionalProgram.executableEncoding.askForAge)
    val p3: declarativeEncoding.Console[Unit] = functionalProgram.declarativeEncoding.greet2
    functionalProgram.declarativeEncoding.interpret(p3)
  }
}