import java.time.{Duration, OffsetDateTime}

package object scheduler {

  // повторять с заданным интервалом
  // повторять заданное количество раз
  // Повторять в определенный момент времени
  // день недели
  // час дня
  // минута часа

  // 1. step: OffsetDateTime => Boolean // вопрос с чем продолжать?
  // 2. step: OffsetDateTime => Decision (Done | Continue)

  type Step = OffsetDateTime => Decision

  sealed trait Decision
  case object Done extends Decision
  case class Continue(interval: OffsetDateTime, next: Step) extends Decision

  case class Scheduler(step: Step){self =>

    // Возвращает новый Schedule, которое объединяет этот с указанным.
    // Продолжаем до тех пор, пока оба Schedule хотят продолжать, и объединяем
    // следующие интервалы в соответствии с указанной функцией слияния.
    def intersect(that: Scheduler)(f: (OffsetDateTime, OffsetDateTime) => OffsetDateTime): Scheduler = {
       def loop(self: Step, that: Step): Step = now => {
         val left  = self(now)
         val right = that(now)
         (left, right) match {
           case (Done, Done) => Done
           case (Done, Continue(_, _)) => Done
           case (Continue(_, _), Done) => Done
           case (Continue(lInterval, lNext), Continue(rInterval, rNext)) =>
             val combined = f(lInterval, rInterval)
             Continue(combined, loop(lNext, rNext))
         }
       }
       Scheduler(loop(self.step, that.step))
     }

    // Возвращает новый Schedule, которое объединяет этот с указанным.
    // Продолжаем до тех пор, пока один из Schedule хочет продолжать, и объединяем
    // следующие интервалы в соответствии с указанной функцией слияния.
    def union(that: Scheduler)(f: (OffsetDateTime, OffsetDateTime) => OffsetDateTime): Scheduler = {
      def loop(self: Step, that: Step): Step = now => {
        val left = self(now)
        val right = that(now)
        (left, right) match {
          case (Done, Done) => Done
          case (Done, Continue(rInterval, next)) => Continue(rInterval, loop(_ => Done, next))
          case (Continue(lInterval, next), Done) => Continue(lInterval, loop(_ => Done, next))
          case (Continue(lInterval, lNext), Continue(rInterval, rNext)) =>
            val combined = f(lInterval, rInterval)
            Continue(combined, loop(lNext, rNext))
        }
      }
      Scheduler(loop(self.step, that.step))
    }

    def andThen(that: Scheduler): Scheduler = {
      def loop(self: Step, that: Step): Step = now => {
        self(now) match {
          case Done => that(now) match {
            case Done => Done
            case Continue(interval, next) => Continue(interval, loop(next, self))
          }
          case Continue(interval, next) => Continue(interval, loop(next, that))
        }
      }
      Scheduler(loop(self.step, that.step))
    }

    private def maxOffsetDateTime(l: OffsetDateTime, r: OffsetDateTime): OffsetDateTime =
      if (l.compareTo(r) >= 0) l else r
    private def minOffsetDateTime(l: OffsetDateTime, r: OffsetDateTime): OffsetDateTime =
      if (l.compareTo(r) >= 0) r else l

    def &&(that: Scheduler): Scheduler = intersect(that)((l, r) => maxOffsetDateTime(l, r))

    def ||(that: Scheduler): Scheduler = union(that)((l, r) => minOffsetDateTime(l, r))

    def schedule(f: => Unit): Unit = {
      def loop(decision: Decision): Decision = decision match {
         case Done => Done
         case c @ Continue(interval, next) =>
           if(OffsetDateTime.now().toInstant.toEpochMilli >= interval.toInstant.toEpochMilli) {
             f
             loop(next(interval))
           } else loop(c)
      }
      loop(self.step(OffsetDateTime.now()))
    }
  }


  object Scheduler{

    def recurs(n: Int): Scheduler = {
      def loop(i: Int): Step = now => {
        if(i <= 0) Done else Continue(now, loop(i - 1))
      }
      Scheduler(loop(n))
    }

    def fixed(duration: Duration): Scheduler = {

      case class State(startMillis: Long, lastRun: Long)

      def loop(state: Option[State]): Step = now => {
        state match {
          case Some(State(startMillis, lastRun)) =>
            val intervalMillis = duration.toMillis
            val nowMillis = now.toInstant.toEpochMilli
            val runningBehind = nowMillis > (lastRun + intervalMillis)
            val nextRun = if (runningBehind) now else now.plus(duration)
            Continue(nextRun, loop(Some(State(startMillis, nextRun.toInstant.toEpochMilli))))
          case None =>
            val nextRun = now.plus(duration)
            Continue(nextRun, loop(Some(State(now.toInstant.toEpochMilli,
              nextRun.toInstant.toEpochMilli))))
        }
      }
      new Scheduler(loop(None))
    }
  }
}
