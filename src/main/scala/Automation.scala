import com.github.nscala_time.time.Imports.{LocalTime, richAbstractPartial}
import hass.controller.{Channel, Hass}
import hass.model.entity.{Entity, InputBoolean, InputDateTime, Light, Sensor, Sun, Switch, Turnable}
import hass.model.event.{StateChangedEvent, UnknownEvent}
import hass.model.group.SwitchesGroup
import hass.model.state.{InputBooleanState, SensorState, SunState, UnknownEntityState}
import hass.model.state.ground.{AboveHorizon, BelowHorizon, Horizon, Off, On, Time, TurnState}
import org.joda.time.{DateTime, DateTimeConstants}

import scala.concurrent.duration._
import scala.concurrent.duration._

object Automation extends App {

  val HASS_URI = "192.168.1.10:8123"
  val source = scala.io.Source.fromFile("jwt.txt")
  val token = source.mkString.filter(_ != '\n')
  source.close()

  implicit val hass: Hass = Hass(HASS_URI, token)
  def log(s: String): Unit = hass.log.inf(s)
  Pozzo.run(hass)

  val sun = Sun()
  val time = Sensor()
  val frontDoorLight = Switch("luce_fuori_porta_davanti")
  val frontLights = Switch("luce_fuori_davanti")
  val gateLights = Switch("luce_cancello")
  val backLights = Switch("luce_fuori_dietro")
  val sideLights = Switch("luce_fuori_dietro_lato")
  val allOutdoorLights = SwitchesGroup(Seq(frontDoorLight, frontLights, gateLights, backLights, sideLights))
  val outdoorLightsToShutdown = SwitchesGroup(Seq(frontLights, gateLights, backLights, sideLights))
  val nightDelayOn = 30.minutes
  val savingDelayOn = 30.minutes
  val lightsChannel = Channel("lightsChannel")


  log("Outdoor light automation on.")
  time.onValueChange {
    case (_, date1, date2) if date1 != date2 && date2 == "23:00" =>
      lightsChannel.signal(3)
  }
  sun.onValueChange {
    case (_, AboveHorizon, BelowHorizon) =>
      log("Outdoor lights will turn on in " + nightDelayOn)
      lightsChannel.signal(1, nightDelayOn)
  }
  sun.onValueChange {
    case (_, BelowHorizon, AboveHorizon) =>
      lightsChannel.signal(4)
  }

  lightsChannel.onSignal(c => {
    case 1 =>
      log("All outdoor lights on")
      allOutdoorLights.turnOn()
      c.signal(2, savingDelayOn)
    case 2 =>
      log("Turn off some light for saving...")
      if(gateLights.value.contains(On)) {
        frontLights.turnOff()
      }
      if(backLights.value.contains(On)) {
        sideLights.turnOff()
      }
    case 3 =>
      log("Keeping only front door light...")
      outdoorLightsToShutdown.turnOff()
      frontDoorLight.turnOn()
    case 4 =>
      log("Dawn. turning off al lights...")
      allOutdoorLights.turnOff()
  })

}
