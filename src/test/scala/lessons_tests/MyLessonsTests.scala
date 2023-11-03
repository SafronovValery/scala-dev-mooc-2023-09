package lessons_tests

import module1.futures.task_futures_sequence
import module1.promise.FutureSyntax._
import module1.threads.ToyFuture
import module1.utils.NameableThreads
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util._

class MyLessonsTests extends AnyFlatSpec  with Matchers {

  def await[A](future: Future[A]): A = Await.result(future, Duration.Inf)

  def awaitEither[A](future: Future[A]): Either[String,A] = {
    Try{Await.result(future, Duration.Inf)} match {
      case Failure(exception) => Left(exception.getMessage)
      case Success(value) => Right(value)
    }

  }

  def fut(i: Int)(implicit ex: ExecutionContext): Future[Int] = Future {
    Thread.sleep(1000)
    i
  }

  val pool: ExecutorService =
    Executors.newCachedThreadPool(NameableThreads("pool"))
  implicit val ecx = ExecutionContext.fromExecutor(pool)

  "full sequence" should "process list of success module1.futures" in {
    val fut1 = fut(1)
    val fut2 = fut(2)
    val fut3 = fut(3)

    assert(await(task_futures_sequence.fullSequence[Int](List(fut1, fut2, fut3))) === (List(1, 2, 3), List()))
  }

  it should "process list of success and failures" in {
    val ex1 = new Exception("ex1")
    val ex2 = new Exception("ex2")
    val failed1 = Future.failed(ex1)
    val failed2 = Future.failed(ex2)
    val fut1 = fut(1)

    assert(await(task_futures_sequence.fullSequence[Int](List(fut1, failed1, failed2))) === (List(1), List(ex1, ex2)))
  }

  it should "process list of failures" in {
    val ex1 = new Exception("ex1")
    val ex2 = new Exception("ex2")
    val failed1 = Future.failed(ex1)
    val failed2 = Future.failed(ex2)

    assert(await(task_futures_sequence.fullSequence[Int](List(failed1, failed2))) === (List(), List(ex1, ex2)))
  }

  //////////////////////// тесты для реализации 2

  "full sequence 2" should "process list of success module1.futures" in {
    val fut1 = fut(1)
    val fut2 = fut(2)
    val fut3 = fut(3)

    assert(await(task_futures_sequence.fullSequence_2[Int](List(fut1, fut2, fut3))) === (List(1, 2, 3), List()))
  }

  it should "process list of success and failures" in {
    val ex1 = new Exception("ex1")
    val ex2 = new Exception("ex2")
    val failed1 = Future.failed(ex1)
    val failed2 = Future.failed(ex2)
    val fut1 = fut(1)

    assert(await(task_futures_sequence.fullSequence_2[Int](List(fut1, failed1, failed2))) === (List(1), List(ex1, ex2)))
  }


  it should "process list of failures" in {
    val ex1 = new Exception("ex1")
    val ex2 = new Exception("ex2")
    val failed1 = Future.failed(ex1)
    val failed2 = Future.failed(ex2)

    assert(await(task_futures_sequence.fullSequence_2[Int](List(failed1, failed2))) === (List(), List(ex1, ex2)))
  }

  //////////////////////// тесты для реализации 3

  "full sequence 3" should "process list of success module1.futures" in {
    val fut1 = fut(1)
    val fut2 = fut(2)
    val fut3 = fut(3)

    assert(await(task_futures_sequence.fullSequence_3[Int](List(fut1, fut2, fut3))) === (List(1, 2, 3), List()))
  }

  it should "process list of success and failures" in {
    val ex1 = new Exception("ex1")
    val ex2 = new Exception("ex2")
    val failed1 = Future.failed(ex1)
    val failed2 = Future.failed(ex2)
    val fut1 = fut(1)

    assert(await(task_futures_sequence.fullSequence_3[Int](List(fut1, failed1, failed2))) === (List(1), List(ex1, ex2)))
  }


  it should "process list of failures" in {
    val ex1 = new Exception("ex1")
    val ex2 = new Exception("ex2")
    val failed1 = Future.failed(ex1)
    val failed2 = Future.failed(ex2)

    assert(await(task_futures_sequence.fullSequence_3[Int](List(failed1, failed2))) === (List(), List(ex1, ex2)))
  }


  /////////////////// тесты на реализацию методов и функций в 07-async

  "make without timeout" should "match future behavior" in {

    val f1 = make({
      3 + 3
    })

    assert(awaitEither(f1) === Right(6))

    val f2 = make({
      Thread.sleep(2000)
      "добрый " + "день"
    })

    assert(awaitEither(f2) === Right("добрый день"))

  }

  "make with timeout" should "match future behavior and saccess" in {


    val f3 = make({
      Thread.sleep(1000)
      8 + 8
    }, 1500)

    assert(awaitEither(f3) === Right(16))

  }

  it should "match future behavior and failed by timeout" in {

  val f4 = make({
    Thread.sleep(1000)
    "один " + "два"
  }, 100)


    assert(awaitEither(f4) === Left("Timeout Exception"))
}

"FutureSyntax map" should "future map behover" in {

  assert(awaitEither(map[Int,Int](Future(3))(_ * 7)) === Right(21))
  assert(awaitEither(map[String,String](Future("Мы любим "))(_ + "Scala")) === Right("Мы любим Scala"))
  assert(awaitEither(map[String,Int](Future("Functional"))(_.length)) === Right(10))

}

  "FutureSyntax flatMap" should "future flatmap behover" in {
    assert(awaitEither(flatMap[Int,Int](Future(3))(x => Future(x * 7))) === Right(21))

    val code1 = {
      Thread.sleep(300)
      1 + 2
    }


    assert(awaitEither(flatMap[Int,Int](Future(code1))(x => Future(x * 3))) === Right(9))

    assert(awaitEither(flatMap[Int,Int](Future(3))(x => Future(x / 0))) === Left("/ by zero"))

    assert(awaitEither(flatMap[Int,Int](Future(3 / 0))(x => Future(x * 3))) === Left("/ by zero"))

    assert(awaitEither(flatMap[Int,Int](Future({ throw new Exception("Exception test") ; 2}))(x => Future(x * 3)))
      === Left("Exception test"))


  }

  "ToyFuture" should "future method map behover" in {
    ToyFuture(10)(pool) map(_ + 1) onComplete {
      res => if (!(res == 11)) fail("expected 11")
    }

    ToyFuture("один ")(pool) map (_ + "два") onComplete {
      res => if (!(res == "один два")) fail("expected 'один два'")
    }

  }

  it should "future method flatMap behover" in {

    ToyFuture(10)(pool) flatMap( x => ToyFuture(x * 2)(pool)) onComplete {
      res => if (!(res == 20)) fail("expected 20")
    }


    ToyFuture("Вероятно... ")(pool) flatMap( x => ToyFuture(x + "Это истина, брат!")(pool)) onComplete {
      res => if (!(res == "Вероятно... Это истина, брат!")) fail("expected 'Вероятно... Это истина, брат!'")
    }
  }

}
