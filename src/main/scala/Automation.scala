import com.github.nscala_time.time.Imports.{LocalTime, richAbstractPartial}
import hass.controller.{Channel, Hass}
import hass.model.entity.{Entity, InputBoolean, InputDateTime, Light, Switch, Turnable}
import hass.model.event.UnknownEvent
import hass.model.state.{InputBooleanState, SensorState, UnknownEntityState}
import hass.model.state.ground.{Off, On, Time, TurnState}
import org.joda.time.{DateTime, DateTimeConstants}

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
  val ciclo_pozzo_motore = InputBoolean()

  val motore_pozzo = Switch("lampada_edo")
  val irr_davanti = Switch()
  val irr_lato = Switch()
  val irr_dietro = Switch()
  val irr_goccia = Switch()
  val irr_switches = Seq(motore_pozzo, irr_davanti, irr_lato, irr_dietro, irr_goccia)
  val ciclo_inputs = Seq(ciclo_pozzo_motore, ciclo_pozzo_davanti, ciclo_pozzo_lato, ciclo_pozzo_dietro, ciclo_pozzo_goccia)
  val irr_automatica_durata = InputDateTime()
  val time = InputDateTime()
  val irrigazione_inizio = InputDateTime()
  val irrigazione_lunedi = InputBoolean()
  val irrigazione_martedi = InputBoolean()
  val irrigazione_mercoledi = InputBoolean()
  val irrigazione_giovedi = InputBoolean()
  val irrigazione_sabato = InputBoolean()
  val irrigazione_venerdi = InputBoolean()
  val irrigazione_domenica = InputBoolean()
  val DEFAULT_TIME_PER_IRRIGATION = 600.seconds
  val MOTOR_START_TIME = 30.seconds

  def log(s: String): Unit = hass.log.inf(s)

  def timePerCycleIrrigation: FiniteDuration = (irr_automatica_durata.value match {
    case Some(Time(h, m, s)) => h.hours + m.minute + s.seconds
    case _ => DEFAULT_TIME_PER_IRRIGATION
  }) match {
    case d if d.toMillis > 0 => d
    case _ => DEFAULT_TIME_PER_IRRIGATION
  }


  var doing: Option[(Switch, InputBoolean)] = None
  var pending = Seq[(Switch, InputBoolean)]()
  val todosChannel = Channel("todos")

  todosChannel.onSignal(c => {

    case ("Append", sw: Switch, ib: InputBoolean) if doing.isEmpty =>
      motore_pozzo.turnOn()
      doing = Some((sw, ib))
      todosChannel.signal("Motor ok", MOTOR_START_TIME)

    case ("Append", sw: Switch, ib: InputBoolean) if doing.nonEmpty =>
      pending = pending :+ (sw, ib)

    case "Motor ok" => doing match {
      case Some((sw, ib)) =>
        Channel(sw.entityId).signal(On)
        todosChannel.signal(("Remove", sw, ib), timePerCycleIrrigation)
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
          todosChannel.signal(("Remove", sw1, ib1), timePerCycleIrrigation)
      }

    case ("Remove", sw: Switch, ib: InputBoolean) if pending.nonEmpty =>
      pending = pending.filter(_ != (sw, ib))
  })

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

    Channel(sw.entityId).onSignal(c => {
      case On => c.signal("OnReal", 2.seconds); log(sw.entityName + " On in 2 seconds.")
      case "OnReal" => sw.turnOn(); log(sw.entityName + " On!")
      case Off => c.reset(); sw.turnOff(); log(sw.entityName + " Off!")
    })
  }


  def checkAutomaticIrrigation(): Unit = {
    val isToday = DateTime.now.getDayOfWeek match {
      case DateTimeConstants.MONDAY => irrigazione_lunedi.value.contains(On)
      case DateTimeConstants.TUESDAY => irrigazione_martedi.value.contains(On)
      case DateTimeConstants.WEDNESDAY => irrigazione_mercoledi.value.contains(On)
      case DateTimeConstants.THURSDAY => irrigazione_giovedi.value.contains(On)
      case DateTimeConstants.FRIDAY => irrigazione_venerdi.value.contains(On)
      case DateTimeConstants.SATURDAY => irrigazione_sabato.value.contains(On)
      case DateTimeConstants.SUNDAY => irrigazione_domenica.value.contains(On)
    }

    val time = irrigazione_inizio.value match {
      case Some(time: Time) => time.toJoda
      case _ => LocalTime.MIDNIGHT
    }

    val isNow = LocalTime.now > time && time.plusMillis(timePerCycleIrrigation.toMillis.toInt * 3) > LocalTime.now

    if (isToday && isNow) {
      //ciclo_pozzo_goccia.turnOn()
      ciclo_pozzo_davanti.turnOn()
      ciclo_pozzo_lato.turnOn()
      ciclo_pozzo_dietro.turnOn()
    }
  }

  hass.onStateChange {
    case SensorState("time", state, lastChanged, lastUpdated, attributes) =>
      checkAutomaticIrrigation()
  }


  //DEBUG
  irrigazione_inizio.onState {
    case (date, _, _) =>
      log("Inizio irrigazione automatica: " + date)
      checkAutomaticIrrigation()
  }
  irr_automatica_durata.onState {
    case (date, _, _) =>
      log("Tempo per lato: " + date)
      checkAutomaticIrrigation()
  }
  irrigazione_lunedi.onState {
    case (state, _, _) =>
      log("lunedì: " + state)
      checkAutomaticIrrigation()
  }
  irrigazione_martedi.onState {
    case (state, _, _) =>
      log("martedì: " + state)
      checkAutomaticIrrigation()
  }
  irrigazione_mercoledi.onState {
    case (state, _, _) =>
      log("mercoledì: " + state)
      checkAutomaticIrrigation()
  }
  irrigazione_giovedi.onState {
    case (state, _, _) =>
      log("giovedi: " + state)
      checkAutomaticIrrigation()
  }
  irrigazione_venerdi.onState {
    case (state, _, _) =>
      log("venerdi: " + state)
      checkAutomaticIrrigation()
  }
  irrigazione_sabato.onState {
    case (state, _, _) =>
      log("sabato: " + state)
      checkAutomaticIrrigation()
  }
  irrigazione_domenica.onState {
    case (state, _, _) =>
      log("domenica: " + state)
      checkAutomaticIrrigation()
  }
}
