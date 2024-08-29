package emailFilter

case class EmailFilters[T](run: (T, Email) => Boolean){self =>

  def zip[B](that: EmailFilters[B]): EmailFilters[(T, B)] = EmailFilters[(T, B)] { case ((t, b), e) =>
    self.run(t, e) && that.run(b, e)
  }

  def &&(that: EmailFilters[T]): EmailFilters[T] = EmailFilters(
    (t, e) => self.run(t, e) && that.run(t, e)
  )

  def ||(that: EmailFilters[T]): EmailFilters[T] = EmailFilters(
    (t, e) => self.run(t, e) || that.run(t, e)
  )
}

object EmailFilters{
  val bySender : EmailFilters[String] = EmailFilters((sender, e) => e.from == sender)
  val byTitle: EmailFilters[String] = EmailFilters((title, e) => e.title == title)
  val bySenderAndTitle = bySender zip byTitle
}