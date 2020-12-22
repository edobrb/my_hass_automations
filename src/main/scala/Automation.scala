import hass.controller.Hass


trait Automation {
  def hass: Hass

  def log(s: String): Unit = hass.log.inf(s)
}

object Automation extends App {
  val HASS_URI = "192.168.1.10:8123"
  val source = scala.io.Source.fromFile("jwt.txt")
  val token = source.mkString.filter(_ != '\n')
  source.close()

  val hass: Hass = Hass(HASS_URI, token)
  WaterWell(hass).run()
  Conditioner(hass).run()
}
