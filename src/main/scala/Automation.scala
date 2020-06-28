import hass.controller.Hass
import hass.model.entity.{InputBoolean, Switch}
import hass.model.group.SwitchesGroup
import hass.model.state.ground.{Off, On}

import scala.concurrent.duration._

object Automation extends App {

  val HASS_URI = "192.168.1.10:8123"
  val source = scala.io.Source.fromFile("jwt.txt")
  val token = source.mkString
  source.close()
  implicit val hass: Hass = Hass(HASS_URI, token)

  val ciclo_pozzo = InputBoolean()
  val motore_pozzo = Switch()
  val irr_davanti = Switch()
  val irr_lato = Switch()
  val irr_dietro = Switch()
  val TIME_PER_IRRIGATION = 10.minutes
  val irrigations = SwitchesGroup(Seq(motore_pozzo, irr_davanti, irr_lato, irr_dietro))

  val irrChannel = hass.channel("irr channel")

  def log(s: String): Unit = println(s)

  ciclo_pozzo.onState {
    case (On, _, _) =>
      irrigations.turnOff()
      motore_pozzo.turnOn()
      log("Avvio motore...")
      irrChannel.signal("Turn On 1", 30.seconds)

    case (Off, _, _) =>
      irrChannel.reset()
      irrigations.turnOff()
      log("Terminated.")
  }

  irrChannel.onSignal {
    case "Turn On 1" =>
      irr_davanti.turnOn()
      irrChannel.signal("Turn Off 1", TIME_PER_IRRIGATION)
    case "Turn Off 1" =>
      irr_davanti.turnOff()
      irrChannel.signal("Turn On 2", 2.seconds)

    case "Turn On 2" =>
      irr_lato.turnOn()
      irrChannel.signal("Turn Off 2", TIME_PER_IRRIGATION)
    case "Turn Off 2" =>
      irr_lato.turnOff()
      irrChannel.signal("Turn On 3", 2.seconds)

    case "Turn On 3" =>
      irr_dietro.turnOn()
      irrChannel.signal("Terminated", TIME_PER_IRRIGATION)
    case "Terminated" =>
      ciclo_pozzo.turnOff()
  }
  irrChannel.onSignal {
    case v => log(v.toString)
  }
}
