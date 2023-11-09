package module2




import scala.language.reflectiveCalls
import scala.util._

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}

  //tuplef для реализации1
  def tuplef[F[_] , A, B](fa: F[A], fb: F[B])(implicit bindCore: BindCore[F]): F[(A, B)] =
    bindCore.flatMap(fa)(ax =>  bindCore.map(fb)((ax, _)))

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]

  }

  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))


  implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = opt.map(f)
    override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }

  implicit def listBindable[A](list: List[A]): Bindable[List, A] = new Bindable[List, A] {
    override def map[B](f: A => B): List[B] = list.map(f)
    override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
  }

  //реализация 1
  //трейт позволяет реализовать имплицитные значения конвертеров


  trait BindCore[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  }

  object BindCore {
    def apply[F[_]](implicit ev: BindCore[F]): BindCore[F] = ev

  }

  implicit val  optBind: BindCore[Option] = new BindCore[Option] {
    override def map[A, B](optA: Option[A])(f: A => B): Option[B] = optA.map(f)
    override def flatMap[A, B](optA: Option[A])(f: A => Option[B]): Option[B] = optA.flatMap(f)

  }
  implicit val listBind:BindCore[List] = new BindCore[List] {
    override def map[A, B](listA: List[A])(f: A => B): List[B] = listA.map(f)
    override def flatMap[A, B](listA: List[A])(f: A => List[B]): List[B] = listA.flatMap(f)


  }

  //ревлизация 2
  //используем таки предложенный выше  trait Bindable[F[_], A]
  trait BindCore2[F[_]] {
    def bindable[A](v: F[A]): Bindable[F, A]
  }


   object BindCore2 {

    def apply[F[_]](implicit ev: BindCore2[F]): BindCore2[F] = ev

     implicit def optBind2: BindCore2[Option] = new BindCore2[Option] {
       override def bindable[A](v: Option[A]): Bindable[Option, A] = v
     }


     implicit def listBind2: BindCore2[List] = new BindCore2[List] {
       override def bindable[A](v: List[A]): Bindable[List, A] = v
     }

  }

  implicit class BindImplicit[F[_], A](v: F[A]) {
    def bindable(implicit ev: BindCore2[F]): Bindable[F, A] = ev.bindable(v)
  }

 def tuplef_2[F[_]: BindCore2, A, B](fa: F[A], fb: F[B]): F[(A, B)] = fa.bindable.flatMap(ax => fb.bindable.map(bx => (ax,bx)))

  //точный аналог на forcomprehension
  def tuplef_3[F[_] : BindCore2, A, B](fa: F[A], fb: F[B]): F[(A, B)] = for {
    ax <- fa.bindable
    bx <- fb.bindable
  } yield (ax, bx)

////////////////////////////////////////////////////////////////////////////////////////////


  implicit def createBindable[F[_] : BindCore2, A](box: F[A]): Bindable[F, A] = new Bindable[F, A] {
    override def map[B](f: A => B): F[B] = box.bindable.map(f)

    override def flatMap[B](f: A => F[B]): F[B] = box.bindable.flatMap(f)
  }

  def tuplef_4[F[_]:BindCore2,A,B](fa: F[A], fb: F[B]): F[(A, B)] = tupleBindable(fa,fb)


    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)

    val r3: Option[(Int, Int)] = tupleBindable(optBindable(optA), optBindable(optB))
    lazy val r4 = println(tupleBindable(listBindable(list1), listBindable(list2)))


    lazy val r1 = println(tuplef(optA, optB))
    lazy val r2 = println(tuplef(list1, list2))

    lazy val r1_2 = println(tuplef_2(optA, optB))
    lazy val r2_3 = println(tuplef_3(list1, list2))
    lazy val r2_4 = println(tuplef_4(list1, list2))

    // проверка написана в Main
    // Unit тесты написаны в src/test/scala/homework_tests/homework_tests.scala
  }
