package module1.futures

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util._

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    //не стэкобезопасная рекурсия
    def recursion[A](fut: List[Future[A]], acc: List[Either[Throwable, A]])(implicit ex: ExecutionContext): Future[List[Either[Throwable, A]]] = {
      fut match {
        case Nil => Future(acc)
        case h :: t =>
          h transformWith {
            case Success(value) => recursion(t, Right(value) :: acc)
            case Failure(exception) => recursion(t, Left(exception) :: acc)
          }

      }
    }

    recursion(futures, List())
      .flatMap { eithers =>
        Future {
          (eithers.reverse.collect { case Right(r) => r }, eithers.reverse.collect { case Left(l) => l })
        }
      }

  }


  ////// реализация 2

  def fullSequence_2[A](futures: List[Future[A]])
                       (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    //стэкобезопасная рекурсия
    @tailrec
    def recursion[A](fut: List[Future[A]], facc: Future[List[Either[Throwable, A]]])(implicit ex: ExecutionContext): Future[List[Either[Throwable, A]]] = {
      fut match {
        case Nil => facc
        case h :: t =>
          val concat: Future[List[Either[Throwable, A]]] = h transformWith {
            case Success(value) => facc.flatMap(acc => Future(Right(value) :: acc))
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

  //////// реализация 3

  def fullSequence_3[A](futures: List[Future[A]])
                       (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {


    val futureBacOrder: Future[(List[A], List[Throwable])] = futures.foldLeft(Future.successful(List[A](), List[Throwable]())) {
      (lst, ftr) =>
        lst.flatMap {
          case (listA, listE) =>
            ftr
              .map { success => (success :: listA, listE) } //добавляем в голову списка, но получем обратный порядок
              .recover { case trw: Throwable => (listA, trw :: listE) } //добавляем в голову списка, но получем обратный порядок
        }
    }

    //реверсируем порядок в списках
    for {
      tupl <- futureBacOrder
    } yield (tupl._1.reverse, tupl._2.reverse)
  }





}
