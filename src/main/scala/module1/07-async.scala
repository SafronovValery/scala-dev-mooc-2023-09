package module1

import module1.utils.NameableThreads

import java.io.File
import java.util.concurrent.{Executor, ExecutorService, Executors, LinkedBlockingQueue}
import java.util.{Timer, TimerTask}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.io.{BufferedSource, Source}
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

object threads {


  // Thread

  class Thread1 extends Thread{
    override def run(): Unit = {
      println(s"Hello from [${Thread.currentThread().getName}]")
    }
  }



  def printRunningTime(v: => Unit): Unit = {
    val start = System.currentTimeMillis()
    v
    val end = System.currentTimeMillis()
    println(s"Execution time ${end - start}")
  }

  // rates

  def getRatesLocation1 = {
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }

  def getRatesLocation2 = {
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }


  // async

  def async(f: => Unit): Thread = new Thread{
    override def run(): Unit = f
  }

  def getRatesLocation3 = async{
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }

  def getRatesLocation4 = async{
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }

  def async2[A](f: => A): A = {
    var r: A = null.asInstanceOf[A]
    val t = new Thread{
      override def run(): Unit = r = f
    }
    t.start()
    t.join()
    r
  }

  def getRatesLocation5 = async2{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }

  def getRatesLocation6 = async2{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }


  class Foo[T](v: => T){
    v
  }




  class ToyFuture[T] private( v: => T){

    private var isCompleted: Boolean = false
    private var r: T = null.asInstanceOf[T]
    private val q = mutable.Queue[T => _]()


    //реализован
    def map[B](f: T => B)(implicit  executor: Executor): ToyFuture[B] = ToyFuture[B] {

      val innerQ = new LinkedBlockingQueue[B]
      this.onComplete {
        res => innerQ.put(f(res))
      }

      innerQ.take()
    } (executor)



    //реализован
    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = f(v)


    def onComplete[U](f: T => U): Unit = {
        if(isCompleted) f(r)
        else q.enqueue(f)
    }

    private def start(executor: Executor) = {
      val t = new Runnable {
        override def run(): Unit = {
          r = v
          isCompleted = true
          while (q.nonEmpty){
            q.dequeue()(r)
          }
        }
      }
      executor.execute(t)
    }
  }

  object ToyFuture{
    def apply[T](v: => T)(executor: Executor): ToyFuture[T] = {
      val f = new ToyFuture[T](v)
      f.start(executor)
      f
    }
  }


  val getRatesLocation7: ToyFuture[Int] = ToyFuture{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }(executor.pool1)

  val getRatesLocation8: ToyFuture[Int] = ToyFuture{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }(executor.pool1)


}

object executor {
      val pool1: ExecutorService =
        Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
      val pool2: ExecutorService =
        Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
      val pool3: ExecutorService =
        Executors.newWorkStealingPool(4)
      val pool4: ExecutorService =
        Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}

object try_{

  def readFromFile(): List[String] = {
    val s: BufferedSource = Source.fromFile(new File("ints.txt"))
    try{
      s.getLines().toList
    } catch { case e =>
        println(e.getMessage)
        Nil
    } finally {
      s.close()
    }
  }

  def readFromFile2: Try[List[Int]] = {
    val s: BufferedSource = Source.fromFile(new File("ints.txt"))
    val r: Try[List[Int]] = Try(s.getLines().toList.map(_.toInt))
    s.close()
    r
  }

  def readFromFile3(): Try[List[Int]] = {
    val source: Try[BufferedSource] = Try(Source.fromFile(new File("ints4.txt")))

    def lines(s: Source): Try[List[Int]] = Try(s.getLines().toList.map(_.toInt))

    val r: Try[List[Int]] = for{
      s <- source
      l <- lines(s)
    } yield l
    source.foreach(_.close())
    r
  }

}

object future{
  // constructors

  val f1: Future[Int] = Future(10)(scala.concurrent.ExecutionContext.global)
  val f2: Future[Int] = Future.successful(10)
  val f3 = Future.failed(new Throwable("oops"))

  def getRatesLocation1 = Future{
    Thread.sleep(1000)
    "GetRatesLocation1"
  }(scala.concurrent.ExecutionContext.global)

  def getRatesLocation2 = Future{
    Thread.sleep(2000)
    "GetRatesLocation2"
  }(scala.concurrent.ExecutionContext.global)

  // combinators
  def longRunningComputation: Int = ???
  val f4: Future[Int] = Future(longRunningComputation)(scala.concurrent.ExecutionContext.global)
  val f5 = f4.map(i => i + 10)(scala.concurrent.ExecutionContext.global)

  f4.foreach{ i =>
    println(i)
  }(scala.concurrent.ExecutionContext.global)

  val f6: Future[Int] = f5.recover{
    case e => 0
  }(scala.concurrent.ExecutionContext.global)

  import scala.concurrent.ExecutionContext.Implicits.global

  def printRunningTime[T](v: => Future[T]): Future[T] = for{
    start <- Future.successful(System.currentTimeMillis())
    r <- v
    end <- Future.successful(System.currentTimeMillis())
    _ <- Future.successful(println(s"Execution time ${end - start}"))
  } yield r


  def action(v: Int): Int = {
    Thread.sleep(1000)
    println(s"Action $v in ${Thread.currentThread().getName}")
    v
  }
  // Execution contexts
  val ec = ExecutionContext.fromExecutor(executor.pool1)
  val ec2 = ExecutionContext.fromExecutor(executor.pool2)
  val ec3 = ExecutionContext.fromExecutor(executor.pool3)
  val ec4 = ExecutionContext.fromExecutor(executor.pool4)

  def f7: Future[Int] = Future(action(10))(ec)
  def f8: Future[Int] = Future(action(20))(ec2)

  def f9: Future[Int] = f7.flatMap{ v1 =>
    action(50)
    f2.map{ v2 =>
      action(v1 + v2)
    }(ec4)
  }(ec3)

}

object promise {

  val p1: Promise[Int] = Promise[Int]
  val f1: Future[Int] = p1.future

  object FutureSyntax{
    //реализован
    def map[T, B](future: Future[T])(f: T => B)(implicit exc: ExecutionContext): Future[B] = {
      val p = Promise[B]
      future.onComplete {
        case Failure(exception) => p.failure(exception)
        case Success(value) => p.complete(Try(f(value)))
      }(exc)
      p.future
    }

    //реализован
    def flatMap[T, B](future: Future[T])(f: T => Future[B])(implicit exc: ExecutionContext): Future[B] = {
      val p = Promise[Future[B]]
      future.onComplete {
        case Failure(exception) => p.failure(exception)
        case Success(value) => p.complete(Try(f(value)))
      }(exc)

     p.future.flatten
    }

    //реализован
    def make[T](v: => T)(implicit  ec: ExecutionContext): Future[T] = Future(v)(ec)

    //реализован
    def make[T](v: => T, timeout: Long) (implicit  ec: ExecutionContext): Future[T] = {
      val p = Promise[T]
      val future = Future(v)
      future.onComplete {
        case Failure(exception) => if(!p.isCompleted) p.failure(exception)
        case Success(value) => if(!p.isCompleted) p.complete(Try(value))
      }

      val timer = new Timer(true)
      val task = new TimerTask {
        override def run(): Unit = {
         if(!p.isCompleted) p.failure(new Exception("Timeout Exception"))
        }
      }

      timer.schedule(task, timeout)
      p.future
    }
  }

}