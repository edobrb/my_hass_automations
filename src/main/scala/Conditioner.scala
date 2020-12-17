import java.util.concurrent.TimeUnit

import hass.controller.{Channel, Hass}
import _root_.hass.model.entity.{Script, Sensor}
import org.joda.time.DateTime
import scala.concurrent.duration._
import scala.concurrent.duration.{Duration, FiniteDuration}

case class Conditioner(hass: Hass) extends Automation {
  def run(): Unit = {
    implicit val implicitHass: Hass = hass
    val consumo_totale = Sensor()
    val edo_stanza_temperature = Sensor()
    val turn_on_conditioner_31_c = Script()
    val turn_off_conditioner = Script()
    val commands = Channel("commands")

    hass.onConnection(() => {
      turn_off_conditioner.trigger()
    })

    var isConditionerOn = false
    var lastAction = DateTime.now().minusHours(1)

    def t: FiniteDuration = Duration.apply(DateTime.now().getMillis - lastAction.getMillis, TimeUnit.MILLISECONDS)

    consumo_totale.onStateChange { case _ =>
      for (power <- consumo_totale.numericValue;
           temp <- edo_stanza_temperature.numericValue)
        yield {
          if (!isConditionerOn && power > 1000 && temp < 18) {
            commands.signal("on hot")
          }
          if (isConditionerOn && (power < -300 || temp > 20)) {
            commands.signal("off")
          }
        }
    }

    commands.onSignal(_ => {
      case "on hot" if t > 10.minutes =>
        log(s"Turning on conditioner to hot (${consumo_totale.numericValue} ${edo_stanza_temperature.numericValue})")
        turn_on_conditioner_31_c.trigger()
        isConditionerOn = true
        lastAction = DateTime.now()
      case "off" if t > 10.minutes =>
        log(s"Turning off (${consumo_totale.numericValue} ${edo_stanza_temperature.numericValue})")
        turn_off_conditioner.trigger()
        isConditionerOn = false
        lastAction = DateTime.now()
    })
  }
}
