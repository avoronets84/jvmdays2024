import java.io.{BufferedReader, Closeable, FileReader}
import scala.util.{Failure, Success, Try}

package object resourses {

  def main(args: Array[String]): Unit = {

    def withResource[T <: Closeable, R](resource: T)(f: T => R): R = {
      try {
        f(resource)
      } finally {
        resource.close()
      }
    }

    def lines(reader: BufferedReader): Iterator[String] =
      Iterator.continually(reader.readLine()).takeWhile(_ != null)

    def resource(fileName: String) = new BufferedReader(new FileReader(fileName))

    withResource(resource("file1.txt")) {
      r1 =>
        withResource(resource("file2.txt")) { r2 =>
          (lines(r1) ++ lines(r2)).foreach(println)
        }
    }

    class ManagedResource[R](val acquire: () => R,
                             val release: R => Any) {
      self =>
      def use[A](f: R => A): Try[A] = {
        Try(acquire()) match {
          case Failure(exception) => Failure(exception)
          case Success(value) =>
            val result = Try(f(value))
            Try(release(value))
            result
        }
      }

      def zip[R2](that: ManagedResource[R2]): ManagedResource[(R, R2)] =
        ManagedResource((this.acquire(), that.acquire())) { case (a, b) =>
          that.release(b)
          this.release(a)
        }

      def flatMap[B](f: R => ManagedResource[B]): ManagedResource[B] = ???
    }

    object ManagedResource {
      def apply[R](acquire: => R)(release: R => Any): ManagedResource[R] =
        new ManagedResource(
          () => acquire, release
        )
    }

    val mr1 = ManagedResource(new BufferedReader(
      new FileReader("file.txt")))(_.close())
    val mr2 = ManagedResource(new BufferedReader(
      new FileReader("file.txt")))(_.close())

    val mr3 = mr1 zip mr2
    mr3.use { case (r1, r2) =>
      (lines(r1) ++ lines(r2)).foreach(println)
    }

    mr1.use(r1 => lines(r1).foreach(println))
  }
}
