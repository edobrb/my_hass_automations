import com.github.nscala_time.time.Imports.LocalTime
import hass.controller.{Channel, Hass}
import hass.model.entity.{InputBoolean, InputDateTime, Switch}
import hass.model.state.SensorState
import hass.model.state.ground.{Off, On, Time}
import org.joda.time.{DateTime, DateTimeConstants}
import scala.concurrent.duration._
import scala.concurrent.duration.FiniteDuration

case class WaterWell(hass: Hass) extends Automation {
  def run(): Unit = {
    implicit val implicitHass: Hass = hass
    val startTime = DateTime.now()

    val ciclo_pozzo_goccia = InputBoolean()
    val ciclo_pozzo_davanti = InputBoolean()
    val ciclo_pozzo_lato = InputBoolean()
    val ciclo_pozzo_dietro = InputBoolean()
    val ciclo_pozzo_motore = InputBoolean()

    val motore_pozzo = Switch()
    val irr_davanti = Switch()
    val irr_lato = Switch()
    val irr_dietro = Switch()
    val irr_goccia = Switch()
    val irr_switches = Seq(motore_pozzo, irr_davanti, irr_lato, irr_dietro, irr_goccia)
    val ciclo_inputs = Seq(ciclo_pozzo_motore, ciclo_pozzo_davanti, ciclo_pozzo_lato, ciclo_pozzo_dietro, ciclo_pozzo_goccia)
    val irr_automatica_durata = InputDateTime()

    val irrigazione_inizio = InputDateTime()
    val irrigazione_lunedi = InputBoolean()
    val irrigazione_martedi = InputBoolean()
    val irrigazione_mercoledi = InputBoolean()
    val irrigazione_giovedi = InputBoolean()
    val irrigazione_sabato = InputBoolean()
    val irrigazione_venerdi = InputBoolean()
    val irrigazione_domenica = InputBoolean()

    val irrigazione_pozzo = InputBoolean()
    val irrigazione_goccia = InputBoolean()
    val irrigazione_davanti = InputBoolean()
    val irrigazione_lato = InputBoolean()
    val irrigazione_dietro = InputBoolean()

    val DEFAULT_TIME_PER_IRRIGATION = 600.seconds
    val MOTOR_START_TIME = 60.seconds
    val SWITCH_DELAY = 800.millisecond

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
      ib.onValue {
        case (_, On) =>
          log(ib.entityId + " On")
          todosChannel.signal(("Append", sw, ib), 0 seconds)
        case (eventTime, Off) if eventTime isAfter startTime =>
          log(ib.entityId + " Off")
          if (doing.contains((sw, ib))) {
            todosChannel.reset()
          }
          todosChannel.signal(("Remove", sw, ib), 0 seconds)
      }

      Channel(sw.entityId).onSignal(c => {
        case On => c.signal("OnReal", SWITCH_DELAY); log(sw.entityName + " On in 2 seconds.")
        case "OnReal" => sw.turnOn(); log(sw.entityName + " On!")
        case Off => c.reset(); sw.turnOff(); log(sw.entityName + " Off!")
      })
    }


    var day = -1

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

      if (isToday) {
        val time = irrigazione_inizio.value match {
          case Some(time: Time) => time.toJoda
          case _ => LocalTime.MIDNIGHT
        }
        val timeNow = hass.stateOf[String, SensorState]("sensor.time").map(_.value).getOrElse("00:00")
        val hourNow = timeNow.split(':')(0).toInt
        val minuteNow = timeNow.split(':')(1).toInt
        if (time.getHourOfDay == hourNow && time.getMinuteOfHour == minuteNow && day != DateTime.now.getDayOfYear) {
          day = DateTime.now.getDayOfYear
          Seq(irrigazione_pozzo, irrigazione_goccia, irrigazione_davanti, irrigazione_lato, irrigazione_dietro).
            map(_.value.contains(On))
            .zip(Seq(ciclo_pozzo_motore, ciclo_pozzo_goccia, ciclo_pozzo_davanti, ciclo_pozzo_lato, ciclo_pozzo_dietro))
            .filter(_._1).map(_._2).foreach(_.turnOn())
        }
      }
    }

    hass.onStateChange {
      case SensorState("time", state, lastChanged, lastUpdated, attributes) =>
        checkAutomaticIrrigation()
    }


    Seq(irrigazione_pozzo, irrigazione_goccia, irrigazione_davanti, irrigazione_lato, irrigazione_dietro).foreach {
      i =>
        i.onValue {
          case (_, _) => day = -1
        }
    }

    //DEBUG
    irrigazione_inizio.onState {
      case (date, _) =>
        log("Inizio irrigazione automatica: " + date)
        checkAutomaticIrrigation()
    }
    irr_automatica_durata.onState {
      case (date, _) =>
        log("Tempo per lato: " + date)
        checkAutomaticIrrigation()
    }
    irrigazione_lunedi.onState {
      case (state, _) =>
        log("lunedì: " + state)
        checkAutomaticIrrigation()
    }
    irrigazione_martedi.onState {
      case (state, _) =>
        log("martedì: " + state)
        checkAutomaticIrrigation()
    }
    irrigazione_mercoledi.onState {
      case (state, _) =>
        log("mercoledì: " + state)
        checkAutomaticIrrigation()
    }
    irrigazione_giovedi.onState {
      case (state, _) =>
        log("giovedi: " + state)
        checkAutomaticIrrigation()
    }
    irrigazione_venerdi.onState {
      case (state, _) =>
        log("venerdi: " + state)
        checkAutomaticIrrigation()
    }
    irrigazione_sabato.onState {
      case (state, _) =>
        log("sabato: " + state)
        checkAutomaticIrrigation()
    }
    irrigazione_domenica.onState {
      case (state, _) =>
        log("domenica: " + state)
        checkAutomaticIrrigation()
    }
    log("Water well automation on.")
  }

}
