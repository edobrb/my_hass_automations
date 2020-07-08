import hass.controller.{Channel, Hass}
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
  val irr_switches = Seq(irr_davanti, irr_lato, irr_dietro, irr_goccia)
  val ciclo_inputs = Seq(ciclo_pozzo_davanti, ciclo_pozzo_lato, ciclo_pozzo_dietro, ciclo_pozzo_goccia)
  val irr_automatica_durata = InputDateTime()
  val DEFAULT_TIME_PER_IRRIGATION = 600.seconds
  val MOTOR_START_TIME = 30.seconds

  def log(s: String): Unit = hass.log.inf(s)

  def timePerIrrigation = (irr_automatica_durata.state.map(_.state) match {
    case Some(Time(h, m, s)) => h.hours + m.minute + s.seconds
    case _ => DEFAULT_TIME_PER_IRRIGATION
  }) match {
    case d if d.toMillis > 0 => d
    case _ => DEFAULT_TIME_PER_IRRIGATION
  }

  var doing: Option[(Switch, InputBoolean)] = None
  var pending = Seq[(Switch, InputBoolean)]()
  val todosChannel = Channel("todos")
  todosChannel.onSignal {

    case ("Append", sw: Switch, ib: InputBoolean) if doing.isEmpty =>
      motore_pozzo.turnOn()
      doing = Some((sw, ib))
      todosChannel.signal("Motor ok", MOTOR_START_TIME)

    case ("Append", sw: Switch, ib: InputBoolean) if doing.nonEmpty =>
      pending = pending :+ (sw, ib)

    case "Motor ok" => doing match {
      case Some((sw, ib)) =>
        Channel(sw.entityId).signal(On)
        todosChannel.signal(("Remove", sw, ib), timePerIrrigation)
      case None =>
        motore_pozzo.turnOff()
    }

    case ("Remove", sw: Switch, ib: InputBoolean) if pending.isEmpty && doing.contains((sw, ib)) =>
      Channel(sw.entityId).signal(Off)
      ib.turnOff()
      motore_pozzo.turnOff()
      doing = None

    case ("Remove", sw: Switch, ib: InputBoolean) if pending.nonEmpty && doing.contains((sw, ib)) =>
      Channel(sw.entityId).signal(Off)
      ib.turnOff()
      pending match {
        case (sw1, ib1) :: tail =>
          pending = tail
          doing = Some((sw1, ib1))
          Channel(sw1.entityId).signal(On)
          todosChannel.signal(("Remove", sw1, ib1), timePerIrrigation)
      }

    case ("Remove", sw: Switch, ib: InputBoolean) if pending.nonEmpty =>
      pending = pending.filter(_ != (sw, ib))
  }
  /*todosChannel.onSignal {
    case v => log(v.toString)
  }*/
  irr_automatica_durata.onState {
    case (date, _, _) => log("New duration: " + date)
  }


  ciclo_inputs.zip(irr_switches).foreach { case (ib, sw) =>
    ib.onState {
      case (On, _, _) =>
        log(ib.entityId + " On")
        todosChannel.signal(("Append", sw, ib), 0 seconds)
      case (Off, eventTime, _) if eventTime isAfter startTime =>
        log(ib.entityId + " Off")
        if (doing.contains((sw, ib))) {
          todosChannel.reset()
        }
        todosChannel.signal(("Remove", sw, ib), 0 seconds)
    }

    val c = Channel(sw.entityId)
    c.onSignal {
      case On => c.signal("OnReal", 2.seconds); log(sw.entityName + " On in 2 seconds.")
      case "OnReal" => sw.turnOn(); log(sw.entityName + " On!")
      case Off => c.reset(); sw.turnOff(); log(sw.entityName + " Off!")
    }
  }

}
