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
    val turn_on_conditioner_16_c_fast = Script()
    val turn_on_conditioner_only_fan = Script()
    val turn_off_conditioner = Script()
    val automate_conditioner = InputBoolean()
    val conditioner_state = InputBoolean()
    val automate_conditioner_after = InputDateTime()
    val automate_conditioner_before = InputDateTime()
    val goCold = InputBoolean("irrigazione_pozzo")

    val commands = Channel("commands")


    hass.onConnection(() => {
      turn_off_conditioner.trigger()
      conditioner_state.turnOff()
    })

    var lastAction = DateTime.now().minusHours(1)
    var job = "NONE"

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
    turn_on_conditioner_16_c_fast.onValue {
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
           before <- automate_conditioner_before.value;
           beforeTime <- Try(before.asInstanceOf[Time]).toOption;
           power <- consumo_totale.numericValue;
           temp <- edo_stanza_temperature.numericValue)
        yield {
          val canAutomate = DateTime.now().toLocalTime.compareTo(afterTime.toJoda) > 0 && DateTime.now().toLocalTime.compareTo(beforeTime.toJoda) < 0
          if(canAutomate) {
            if (state == Off && power > 900 && temp < 19) {
              commands.signal(("hot", power, temp))
            }
            if (state == Off &&  power > 900 && temp >= 25 ) {
              commands.signal(("cold", power, temp))
            }
            if (state == On && job != "COOLING_OFF" && (power < -200 || (temp > 21 && job == "HOT") || (temp <= 24 && job == "COLD")) ) {
              commands.signal("cool_off")
            }
          } else if(state == On) {
            commands.signal(("off", power, temp))
          }
        }
    }

    commands.onSignal(_ => {
      case ("hot", power, temp) if t > 2.minutes =>
        log(s"Turning on conditioner to hot ($power W / $temp °C)")
        turn_on_conditioner_31_c.trigger()
        lastAction = DateTime.now()
        job = "HOT"
      case ("cold", power, temp) if t > 2.minutes =>
        log(s"Turning on conditioner to cold ($power W / $temp °C)")
        turn_on_conditioner_16_c_fast.trigger()
        lastAction = DateTime.now()
        job = "COLD"
      case ("off", power, temp) =>
        log(s"Turning off ($power W / $temp °C)")
        turn_off_conditioner.trigger()
        lastAction = DateTime.now()
        job = "NONE"
      case "cool_off" =>
        log(s"Cooling off")
        turn_on_conditioner_only_fan.trigger()
        job = "COOLING_OFF"
        commands.signal(("off", 0, 0), 1.minutes)
      case v => //log(s"Ignored $v")
    })
  }
}
