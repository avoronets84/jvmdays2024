import scheduler.Scheduler
import java.time.Duration


object Main {

  def main(args: Array[String]): Unit = {
    val fiveTimes: Scheduler = Scheduler.recurs(5)
    val twoTimes: Scheduler = Scheduler.recurs(2)
    val every1Second: Scheduler = Scheduler.fixed(Duration.ofSeconds(1))
    val every5Second: Scheduler = Scheduler.fixed(Duration.ofSeconds(5))

    //  fiveTimes.schedule(println("Hello from Scheduler"))
    //  every1Second.schedule(println("Hello from Scheduler"))
    //  (fiveTimes && every1Second).schedule(println("Hello world"))
  }

}