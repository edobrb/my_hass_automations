import java.util.concurrent.TimeUnit

import hass.controller.{Channel, Hass}
import _root_.hass.model.entity.{InputBoolean, InputDateTime, Script, Sensor}
import hass.model.state.ground.{Off, On, Time, TimeOrDate}
import org.joda.time.DateTime
import scalaz.concurrent.Task.Try

import scala.concurrent.duration._
import scala.concurrent.duration.{Duration, FiniteDuration}

case class Conditioner(hass: Hass) extends Automation {
  def run(): Unit = {
    implicit val implicitHass: Hass = hass
    val consumo_totale = Sensor()
    val edo_stanza_temperature = Sensor()
    val turn_on_conditioner_31_c = Script()
    val turn_off_conditioner = Script()
    val automate_conditioner = InputBoolean()
    val conditioner_state = InputBoolean()
    val automate_conditioner_after = InputDateTime()
    val commands = Channel("commands")


    hass.onConnection(() => {
      turn_off_conditioner.trigger()
      conditioner_state.turnOff()
    })

    var lastAction = DateTime.now().minusHours(1)

    def t: FiniteDuration = Duration.apply(DateTime.now().getMillis - lastAction.getMillis, TimeUnit.MILLISECONDS)

    conditioner_state.onValue {
      case (_, state) => log(s"Conditioner is $state")
    }
    automate_conditioner.onValue {
      case (_, state) => log(s"Conditioner automation $state")
    }
    automate_conditioner_after.onValue {
      case (_, state) => log(s"Conditioner automation after $state")
    }
    turn_on_conditioner_31_c.onValue {
      case (_, On) => conditioner_state.turnOn()
    }
    turn_off_conditioner.onValue {
      case (_, On) => conditioner_state.turnOff()
    }

    consumo_totale.onState { case _ =>
      for (automate <- automate_conditioner.value; if automate == On;
           state <- conditioner_state.value;
           after <- automate_conditioner_after.value;
           afterTime <- Try(after.asInstanceOf[Time]).toOption;
           power <- consumo_totale.numericValue;
           temp <- edo_stanza_temperature.numericValue)
        yield {
          val canAutomate = DateTime.now().toLocalTime.compareTo(afterTime.toJoda) > 0
          if (state == Off && power > 900 && temp < 19 && canAutomate) {
            commands.signal(("on hot", power, temp))
          }
          if (state == On && (power < -300 || temp > 21)) {
            commands.signal(("off", power, temp))
          }
        }
    }

    commands.onSignal(_ => {
      case ("on hot", power, temp) if t > 5.minutes =>
        log(s"Turning on conditioner to hot ($power W / $temp °C)")
        turn_on_conditioner_31_c.trigger()
        lastAction = DateTime.now()
      case ("off", power, temp) if t > 5.minutes =>
        log(s"Turning off ($power W / $temp °C)")
        turn_off_conditioner.trigger()
        lastAction = DateTime.now()
      case v => log(s"Ignored $v")
    })
  }
}
