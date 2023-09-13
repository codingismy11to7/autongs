package autong

object Utils {

  implicit class RichStr(val s: String) extends AnyVal {
    def toOption: Option[String] = Option(s).map(_.trim).filter(_.nonEmpty)
  }

  def toInt(s: Option[String]): Option[Int] = s.flatMap(s => s.toIntOption)

  def toNonNegInt(s: Option[String]): Option[Int] = toInt(s).filter(_ >= 0)

  private val lookup = Map(
    ""  -> 1.0,
    "k" -> 1e3,
    "m" -> 1e6,
    "g" -> 1e9,
    "t" -> 1e12,
    "p" -> 1e15,
    "e" -> 1e18,
  )

  def getValueWithUnits(amt: Double, units: String): Option[Double] =
    lookup.get(units.toLowerCase).map(_ * amt)

}
