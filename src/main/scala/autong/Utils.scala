package autong

object Utils {

  implicit class RichStr(val s: String) extends AnyVal {
    def toOption: Option[String] = Option(s).map(_.trim).filter(_.nonEmpty)
  }

  def toInt(s: Option[String]): Option[Int] = s.flatMap(s => s.toIntOption)

  def toNonNegInt(s: Option[String]): Option[Int] = toInt(s).filter(_ >= 0)
}
