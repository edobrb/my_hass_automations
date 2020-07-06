import hass.controller.Hass
import hass.model.entity.{InputBoolean, InputDateTime, Switch}
import hass.model.state.InputBooleanState
import hass.model.state.ground.{Off, On, Time, TurnState}
import org.joda.time.DateTime

import scala.concurrent.duration._

object Automation extends App {

  val HASS_URI = "192.168.1.10:8123"
  val source = scala.io.Source.fromFile("jwt.txt")
  val token = source.mkString.filter(_ != '\n')
  source.close()

  implicit val hass: Hass = Hass(HASS_URI, token)
  val startTime = DateTime.now()

  val ciclo_pozzo_goccia = InputBoolean()
  val ciclo_pozzo_davanti = InputBoolean()
  val ciclo_pozzo_lato = InputBoolean()
  val ciclo_pozzo_dietro = InputBoolean()

  val motore_pozzo = Switch()
  val irr_davanti = Switch()
  val irr_lato = Switch()
  val irr_dietro = Switch()
  val irr_goccia = Switch()
  val irr_automatica_durata = InputDateTime()
  val DEFAULT_TIME_PER_IRRIGATION = 600.seconds
  val MOTOR_START_TIME = 30.seconds

  def log(s: String): Unit = println(s)

  def timePerIrrigation = (irr_automatica_durata.state.map(_.state) match {
    case Some(Time(h, m, s)) => h.hours + m.minute + s.seconds
    case _ => DEFAULT_TIME_PER_IRRIGATION
  }) match {
    case d if d.toMillis > 0 => d
    case _ => DEFAULT_TIME_PER_IRRIGATION
  }

  var doing: Option[(Switch, InputBoolean)] = None
  var pending = Seq[(Switch, InputBoolean)]()
  val todosChannel = hass.channel("todos")
  todosChannel.onSignal {

    case ("Append", sw: Switch, ib: InputBoolean) if doing.isEmpty =>
      motore_pozzo.turnOn()
      doing = Some((sw, ib))
      todosChannel.signal("Motor ok", MOTOR_START_TIME)
    case ("Append", sw: Switch, ib: InputBoolean) if doing.nonEmpty =>
      pending = pending :+ (sw, ib)

    case "Motor ok" => doing match {
      case Some((sw, ib)) =>
        sw.turnOn()
        todosChannel.signal(("Remove", sw, ib), timePerIrrigation)
      case None =>
        motore_pozzo.turnOff()
    }

    case ("Remove", sw: Switch, ib: InputBoolean) if pending.isEmpty && doing.contains((sw, ib)) =>
      sw.turnOff()
      ib.turnOff()
      motore_pozzo.turnOff()
      doing = None

    case ("Remove", sw: Switch, ib: InputBoolean) if pending.nonEmpty && doing.contains((sw, ib)) =>
      sw.turnOff()
      ib.turnOff()
      pending match {
        case (sw1, ib1) :: tail =>
          pending = tail
          doing = Some((sw1, ib1))
          sw1.turnOn()
          todosChannel.signal(("Remove", sw1, ib1), timePerIrrigation)
      }

    case ("Remove", sw: Switch, ib: InputBoolean) if pending.nonEmpty =>
      pending = pending.filter(_ != (sw, ib))
  }
  todosChannel.onSignal {
    case v => log(v.toString)
  }
  irr_automatica_durata.onState {
    case (date, time, state) => log("New duration: " + date)
  }

  def f(sw: Switch, ib: InputBoolean): PartialFunction[(TurnState, DateTime, InputBooleanState), Unit] = {
    case (On, _, _) =>
      log(ib.entityName + " On")
      todosChannel.signal(("Append", sw, ib), 0 seconds)
    case (Off, eventTime, _) if eventTime isAfter startTime =>
      log(ib.entityName + " Off")
      if (doing.contains((sw, ib))) {
        todosChannel.reset()
      }
      todosChannel.signal(("Remove", sw, ib), 0 seconds)
  }

  ciclo_pozzo_goccia.onState(f(irr_goccia, ciclo_pozzo_goccia))
  ciclo_pozzo_davanti.onState(f(irr_davanti, ciclo_pozzo_davanti))
  ciclo_pozzo_lato.onState(f(irr_lato, ciclo_pozzo_lato))
  ciclo_pozzo_dietro.onState(f(irr_dietro, ciclo_pozzo_dietro))
}
