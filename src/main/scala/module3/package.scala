import zio.ZIO

package object module3 {

  implicit class ZIOSyntax[-R, +E, +A](z: ZIO[R, E, A]){
    def debug: ZIO[R, E, A] = {
      z.tap(v => ZIO.effect(println(s"[${Thread.currentThread().getName}] $v")).orDie)
    }
  }
}
