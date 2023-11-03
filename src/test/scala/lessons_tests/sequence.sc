import module1.promise.FutureSyntax._
import module1.threads._
import module1.utils.NameableThreads

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util._

val pool: ExecutorService =
  Executors.newFixedThreadPool(100,NameableThreads("pool"))
implicit val ecx = ExecutionContext.fromExecutor(pool)

def fut(i: Int)(implicit ex: ExecutionContext): Future[Int] = Future {
  Thread.sleep(1000)
  i
}

def await[A](future: Future[A]): A = Await.result(future, Duration.Inf)

object  futures {


  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    //не стэкобезопасная рекурсия
    def recursion[A](fut: List[Future[A]], acc: List[Either[Throwable, A]])(implicit ex: ExecutionContext): Future[List[Either[Throwable, A]]] = {
      fut match {
        case Nil => Future(acc)
        case h :: t =>
          h transformWith {
            case Success(value) => recursion(t,   Right(value) :: acc)
            case Failure(exception) => recursion(t, Left(exception) :: acc)
          }

      }
    }
    recursion(futures, List())
      .flatMap { eithers =>
        Future{
          (eithers.reverse.collect { case Right(r) => r }, eithers.reverse.collect { case Left(l) => l })
        }
      }

  }


  def fullSequence_2[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    //стэкобезопасная рекурсия
    @tailrec
    def recursion[A](fut: List[Future[A]], facc: Future[List[Either[Throwable, A]]])(implicit ex: ExecutionContext): Future[List[Either[Throwable, A]]] = {
      fut match {
        case Nil => facc
        case h :: t =>
          val concat: Future[List[Either[Throwable, A]]] =  h transformWith {
            case Success(value) =>  facc.flatMap(acc => Future( Right(value) :: acc  ))
            case Failure(exception) => facc.flatMap(acc => Future(Left(exception) :: acc))
         }

         recursion(t, concat)
      }
    }

    recursion(futures, Future(List()))
      .flatMap { eithers =>
        Future {
          (eithers.reverse.collect { case Right(r) => r }, eithers.reverse.collect { case Left(l) => l })
        }
      }

  }


  def fullSequence_3[A](futures: List[Future[A]])
                       (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {


    val futureBacOrder: Future[(List[A], List[Throwable])] = futures.foldLeft(Future.successful(List[A](), List[Throwable]())) {
      (lst, ftr) => lst.flatMap {
        case (listA, listE) =>
          ftr
            .map { success => ( success :: listA   , listE)} //добавляем в голову списка, но получем обратный порядок
            .recover { case trw: Throwable => (listA, trw :: listE)} //добавляем в голову списка, но получем обратный порядок
      }
    }

    //реверсируем порядок в списках
    for {
        tupl <- futureBacOrder
    } yield (tupl._1.reverse, tupl._2.reverse)
  }

}





val fut1 = fut(1)
val fut2 = fut(2)
val fut3 = fut(3)

await(futures.fullSequence[Int](List(fut1, fut2, fut3)))

val ex1 = new Exception("ex1")
val ex2 = new Exception("ex2")
val failed1 = Future.failed(ex1)
val failed2 = Future.failed(ex2)
val fut1 = fut(1)

await(futures.fullSequence[Int](List(fut1, failed1, failed2)))

await(futures.fullSequence_2[Int](List(fut1, fut2, fut3)))

await(futures.fullSequence_2[Int](List(fut1, failed1, failed2)))

await(futures.fullSequence_3[Int](List(fut1, fut2, fut3)))

await(futures.fullSequence_3[Int](List(fut1, failed1, failed2)))

////////////////////



val tf1 = ToyFuture(1)(pool)

val tf2 = ToyFuture(2)(pool)

val ft3 = ToyFuture("Ля Ля ")(pool)

tf1 map{_ + 1} onComplete {
  res => println(s"ft1 map result = $res")
}

tf2 map (_ * 2) onComplete {
  res => println(s"ft2 map result = $res")
}


ft3 flatMap(x => ToyFuture(x + "Фа")(pool)) onComplete {
  res => println(s"ft3 flat map result = $res")
}

////////////////////////////////////////

make({1 + 1})(ecx).onComplete{
  case Success(value) => println(s" Make $value")
  case Failure(exp) => println(exp.getMessage)
}

/*make({
  Thread.sleep(1000)
  3 + 3})(ecx).onComplete{
  case Success(value) => println(s" Make $value")
  case Failure(exp) => println(exp.getMessage)
}*/

/*make({
  Thread.sleep(1000)
  8 + 2000000000}, 1)(ecx).onComplete{
  case Success(value) => println(s" Make $value")
  case Failure(exp) => println(exp.getMessage)
}*/

Future{
  Thread.sleep(1000)
  2 + 2
} onComplete{
  case Success(value) => println(s" Future $value")
  case Failure(exp) => println(exp.getMessage)
}





