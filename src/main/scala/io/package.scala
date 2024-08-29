import io.declarative

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

package object io {

  val whatIsYourName: Unit = println("Как тебя зовут")
  val name: String = StdIn.readLine()
  val printName: Unit = println(s"Тебя зовут, $name")

  val program = ???

  case class IO[T](run: () => Try[T]){
    def flatMap[B](f: T => IO[B]): IO[B] = IO(() =>{
      this.run() match {
        case Failure(exception) => Failure(exception)
        case Success(value) => f(value).run()
      }
    })

    def map[B](f: T => B): IO[B] = flatMap(v => IO.effect(f(v)))
  }

  object IO{
    def succeed[T](v: T): IO[T] = IO(() => Success(v))
    def effect[T](v: => T): IO[T] = IO(() => Try(v))
    def fail[T](e: Throwable): IO[T] = IO(() => Failure(e))
  }

  object declarative{

    sealed trait IO[+T]{
      def flatMap[B](f: T => IO[B]): IO[B] = FlatMap(this, f)
      def map[B](f: T => B): IO[B] = FlatMap(this, (t: T) => IO.effect(f(t)))
    }

    final case class Effect[T] private(v: () => Try[T]) extends IO[T]
    final case class FlatMap[T, B](v: IO[T], f: T => IO[B]) extends IO[B]


    object IO{
      def effect[T](v: => T): IO[T] = Effect(() => Try(v))
    }

    def run[T, B](io: IO[T]): T = io match {
      case declarative.Effect(v) => v().get
      case declarative.FlatMap(v: IO[T], f: (T => IO[B])) =>
        run(f(run(v)))
    }

    val whatIsYourName: IO[Unit] =
      IO.effect(println("Как тебя зовут?"))
    val name: IO[String] =
      IO.effect(StdIn.readLine())
    val printName: String => IO[Unit] =
      name => IO.effect(println(s"Тебя зовут, $name"))

    val program: IO[Unit] = for{
      _ <- whatIsYourName
      n <- name
      _ <- printName(n)
    } yield ()

    val howOldAreYou: IO[Unit] =
      IO.effect(println("Сколько тебе лет?"))
    val age: IO[Int] =
      IO.effect(StdIn.readLine().toInt)
    val printAge: Int => IO[Unit] =
      name => IO.effect(println(s"Твой возраст, $name"))
  }
}

object App{

  def main(args: Array[String]): Unit = {
    declarative.run(declarative.program)
  }

}