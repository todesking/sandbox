import com.todesking.scalapp.syntax._
import scalaz.Arrow
import scalaz.syntax.arrow._
import scala.specialized

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.existentials

trait RealtimeEvent

object hack {
  type RealtimeEvents = Seq[RealtimeEvent]
}
import hack._

case class SamplingRate(value: Int) extends AnyVal

object Primitive {
  // NOTE: phase is normalized(0.0 to 1.0)
  def freqToPhase(implicit sr: SamplingRate): SignalFunction[Double, Double] =
    SignalFunction.stateOut[Double, Double](0.0) { (freq, state) => (state + freq / sr.value) % 1.0 }

  def clip[@specialized(Int, Double) A](min: A, max: A)(implicit num: Numeric[A]): SignalFunction[A, A] =
    SignalFunction { v => num.max(num.min(v, max), min) }

  def extractEvent[A](pf: PartialFunction[RealtimeEvent, A]): SignalFunction[RealtimeEvents, Seq[A]] =
    SignalFunction[RealtimeEvents, Seq[A]](_.collect(pf)).name("extractEvent")

  def expscale(max: Double): SignalFunction[Double, Double] =
    SignalFunction[Double, Double] { v => math.pow(max, v) }.name(s"expscale(${max})")

  def zeroone[A]: SignalFunction[Seq[A], Option[A]] =
    SignalFunction[Seq[A], Option[A]](_.headOption).name("zeroone")

  def continuous[A](init: A): SignalFunction[Option[A], A] =
    SignalFunction.stateOut[Option[A], A](init) {
      case (Some(ev), st) => ev
      case (None, st) => st
    }.name("continuous")

}

object VCO {
  import Primitive._

  def saw(implicit sr: SamplingRate): SignalFunction[Double, Double] =
    freqToPhase >>> SignalFunction { phase => phase * 2 - 1.0 }

  def sin(implicit sr: SamplingRate): SignalFunction[Double, Double] =
    freqToPhase >>> SignalFunction { phase => math.sin(phase * 2 * math.Pi) }
}

sealed trait MidiEvent extends RealtimeEvent
case class MidiShortMessageEvent(command: Int, channel: Int, data1: Int, data2: Int) extends MidiEvent

object MidiEvent {
}

class World(samplingRate: SamplingRate, bufferNanos: Long, delayNanos: Long) {
  private[this] var lastEventTimestamp: Long = System.nanoTime()
  private[this] var events = scala.collection.immutable.Queue.empty[(RealtimeEvent, Long)]

  def sendEvent(e: RealtimeEvent): Unit = {
    val ts = math.max(System.nanoTime() + delayNanos, lastEventTimestamp)
    synchronized {
      this.events = events.enqueue(e -> ts)
      this.lastEventTimestamp = ts
    }
  }

  private[this] def pollEvent(t0: Long, samples: Long): RealtimeEvents = {
    val clockNanos = (samples.toDouble / samplingRate.value * 1000 * 1000 * 1000).toLong
    val targetNanos = t0 + clockNanos - delayNanos

    while (System.nanoTime() < targetNanos)
      Thread.sleep(0)

    val fired =
      synchronized {
        val es = this.events.takeWhile(_._2 <= targetNanos)
        this.events = this.events.drop(es.size)
        es
      }
    fired.map(_._1)
  }

  def runRealtime(genSF: SamplingRate => SignalFunction[RealtimeEvents, Double]): Unit =
    run(genSF(samplingRate))

  def runUnit(genSF: SamplingRate => SignalFunction[Unit, Double]): Unit =
    run(SignalFunction.ignore[RealtimeEvents] >>> genSF(samplingRate))

  private[this] def run(sf: SignalFunction[RealtimeEvents, Double]): Unit = {
    println(s"Running ${sf.toString}...")
    val proc = sf.buildProc()
    val (line, bufSize) = JavaSound.openLine(samplingRate.value, (bufferNanos / 1000 / 1000).toInt)
    val buffer = new Array[Byte](bufSize)
    val zero = new Array[Byte](bufSize)

    @volatile var shut = false

    val hook = new Thread {
      override def run(): Unit = {
        // TODO: not works, I know...
        shut = true
      }
    }

    Runtime.getRuntime.addShutdownHook(hook)

    val thread = new Thread {
      override def run(): Unit = {
        try {
          val t0 = System.nanoTime()
          var clock: Long = 0L
          while (!shut) {
            for (i <- 0 until buffer.size) {
              val firedEvents = pollEvent(t0, clock)
              val out = proc(firedEvents)
              buffer(i) = (out * 127).toByte
              clock += 1
            }
            if (clock < bufSize * 10)
              line.write(zero, 0, zero.size)
            else
              line.write(buffer, 0, buffer.size)
            // println(s"${line.available()}/${line.getBufferSize}")
          }
          val shutBuf = new Array[Byte]((samplingRate.value * 0.2).toInt)
          for (i <- 0 until shutBuf.size) {
            shutBuf(i) = (buffer.last / i).toByte
          }
          line.write(shutBuf, 0, shutBuf.size)
        } finally {
          println("closing")
          line.flush()
          line.stop()
          line.close()
        }
      }
    }

    thread.run()
    thread.join()
  }
}

object World {
}

object JavaSound {
  import javax.sound.sampled.{ AudioSystem, SourceDataLine, AudioFormat, DataLine }
  import javax.sound.midi.{ MidiSystem, MidiDevice }

  def openLine(samplingRate: Int, bufferSizeMS: Int): (SourceDataLine, Int) = {
    import javax.sound.sampled._
    val channels = 1
    val sampleSize = 1 // byte
    val signed = true
    val bigEndian = false

    val bufferSizeByte = (samplingRate * sampleSize * (bufferSizeMS / 1000.0)).toInt
    val lineBufferSize = bufferSizeByte * 10

    val format = new AudioFormat(samplingRate.toFloat, sampleSize * 8, channels, signed, bigEndian)

    val info = new DataLine.Info(classOf[SourceDataLine], format, lineBufferSize)
    val line = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    line.open(format, lineBufferSize)
    line.start()
    (line, bufferSizeByte)
  }

  def findMidiDevice(name: String, vendor: String): Option[MidiDevice] = {
    MidiSystem.getMidiDeviceInfo()
      .find { di => di.getName == name && di.getVendor == vendor }
      .map { MidiSystem.getMidiDevice(_) }
  }

  def findNanoKontrol2(): Option[NanoKontrol2] = {
    val korg = "KORG INC."
    for {
      in <- findMidiDevice("SLIDER/KNOB", korg)
      out <- findMidiDevice("CTRL", korg)
    } yield NanoKontrol2(in, out)
  }
}

case class NanoKontrol2(in: javax.sound.midi.MidiDevice, out: javax.sound.midi.MidiDevice) {
  import javax.sound.midi.{ MidiMessage, ShortMessage, Receiver, Transmitter }
  def onMessage(f: MidiMessage => Unit): Unit = {
    in.getTransmitter.setReceiver(new Receiver {
      override def send(message: MidiMessage, timestamp: Long): Unit = f(message)
      override def close(): Unit = ()
    })
  }

  def open(): Unit = {
    in.open()
    out.open()
  }

  def close(): Unit = {
    in.close()
    out.close()
  }
}

object NanoKontrol2 {
  import Primitive.{extractEvent, zeroone, continuous}

  class Button(val cc: Int) {
    def toPF: PartialFunction[RealtimeEvent, Boolean] = {
      case MidiShortMessageEvent(cmd, ch, d1, d2) if cc == d1 =>
        d2 == 127
    }

    def event: SignalFunction[RealtimeEvents, Option[Boolean]] =
      extractEvent(toPF) >>>
        zeroone[Boolean]

    def onPush: SignalFunction[RealtimeEvents, Option[Unit]] =
      event >>>
        SignalFunction(_.filter(identity).map { _ => () })
  }
  class Volume(val cc: Int) {
    def toPF: PartialFunction[RealtimeEvent, Double] = {
      case MidiShortMessageEvent(cmd, ch, d1, d2) if cc == d1 =>
        math.min(math.max(d2, 0), 127) / 127.0
    }
    def range(min: Double, max: Double): SignalFunction[RealtimeEvents, Double] =
      extractEvent(toPF) >>>
        zeroone[Double] >>>
        continuous(min) >>>
        SignalFunction { v => min + v * (max - min) }

    def rangeExp(min: Double, max: Double): SignalFunction[RealtimeEvents, Double] =
      range(0.0, 1.0) >>>
        SignalFunction { v => min + math.pow(max - min, v) }

  }

  object transport {
    object track {
      val rew = new Button(58)
      val ff = new Button(59)
    }

    val cycle = new Button(46)

    object marker {
      val set = new Button(60)
      val prev = new Button(61)
      val ff = new Button(62)
    }

    val rew = new Button(43)
    val ff = new Button(44)
    val stop = new Button(42)
    val play = new Button(41)
    val rec = new Button(45)
  }

  class Group(knobId: Int, sliderId: Int, soloId: Int, muteId: Int, recId: Int) {
    val knob = new Volume(knobId)
    val slider = new Volume(sliderId)
    val solo = new Button(soloId)
    val mute = new Button(muteId)
    val rec = new Button(recId)
  }

  val group1 = new Group(16, 0, 32, 48, 64)
  val group2 = new Group(17, 1, 33, 49, 65)
  val group3 = new Group(18, 2, 34, 50, 66)
  val group4 = new Group(19, 3, 35, 51, 67)
  val group5 = new Group(20, 4, 36, 52, 68)
  val group6 = new Group(21, 5, 37, 53, 69)
  val group7 = new Group(22, 6, 38, 54, 70)
  val group8 = new Group(23, 7, 39, 55, 71)
}

object Main {
  def main(args: Array[String]): Unit = {
    val w = new World(SamplingRate(44100), 10L * 1000 * 1000, 20L * 1000 * 1000)
    val nano = JavaSound.findNanoKontrol2()
    nano foreach {
      _.onMessage {
        case sm: javax.sound.midi.ShortMessage =>
          val message = MidiShortMessageEvent(sm.getCommand, sm.getChannel, sm.getData1, sm.getData2)
          w.sendEvent(message)
        case m =>
      }
    }

    import SignalFunction.{ const, ignore,  id,  delay }
    import Primitive.{clip, extractEvent, continuous, zeroone, expscale}
    import ArrowSyntax._

    type SF[A, B] = SignalFunction[A, B]
    try {
      nano foreach { _.open() }
      val eventSink = w.runRealtime { implicit sr =>
        val nk = NanoKontrol2

        def index[F[_, _], A, B](ins: Signal[F, A, B]*)(f: B => Boolean)(implicit a: Arrow[F]): ArrowBuilder[F, A, Option[Int]] =
          ins.zipWithIndex.map {
            case (in, i) =>
              a.arr[B, Option[Int]] { v => if (f(v)) Some(i) else None } -< in
          }.reduceLeft { (l, r) =>
            for {
              sigA <- r
              sigB <- l
              ret <- a.id -< sigA.zip(sigB).map { case (va, vb) => va.orElse(vb) }
            } yield ret
          }

        def selectArrow[F[_, _], A, B](
          arr0: F[A, B],
          arrs: F[A, B]*
        )(implicit Arr: Arrow[F]): F[(Int, A), B] =
          arrs.zipWithIndex.foldLeft[F[(Int, A), B]](
            Arr.arr[(Int, A), A](_._2) >>> arr0
          ) {
              case (arr, (a, i)) =>
                (arr &&& Arr.second[A, B, Int](a)) >>> Arr.arr[(B, (Int, B)), B] {
                  case (a1, (n, a2)) =>
                    if (n == i) a2 else a1
                }
            }

        def lfo(f0: Double)(implicit sr: SamplingRate): SF[(Double, (Double, Double)), Double] = {
          val vt = 40000.0
          val moogAux: SignalFunction[(Double, Double), Double] =
            ArrowBuilder.delayLoop[SignalFunction, (Double, Double), Double, Double](0.0) { in =>
              val x = in.map(_._1._1)
              val g = in.map(_._1._2)
              val ym1 = in.map(_._2)
              for {
                y <- ym1.zip(x).zip(g).map { case ((ym1, x), g) => ym1 + vt * g * (math.tanh(x / vt) - math.tanh(ym1 / vt)) }.arrowBuilder
              } yield y.zip(y)
            }
          ArrowBuilder.build[SignalFunction, (Double, (Double, Double)), Double] { in =>
            val freq = in.map(_._1)
            val resonance = in.map(_._2._1)
            val value = in.map(_._2._2)
            for {
              f <- freq.map { freq => f0 + freq }.arrowBuilder
              g <- f.map { f => 1.0 - math.exp(-2.0 * math.Pi * f / sr.value) }.arrowBuilder
              y <- ArrowBuilder.delayLoop[SignalFunction, (Double, (Double, Double)), Double, Double](0.0) { in =>
                val x = in.map(_._1._1)
                val g = in.map(_._1._2._1)
                val r = in.map(_._1._2._2)
                val y = in.map(_._2)
                for {
                  ya <- {
                    moogAux <<<
                      SignalFunction[((Double, (Double, Double)), Double), (Double, Double)] {
                        case ((x, (g, r)), y) => ((x - 4 * r * y) -> g)
                      }
                  } -< in
                  yb <- moogAux -< (ya -> g)
                  yc <- moogAux -< (yb -> g)
                  yd <- moogAux -< (yc -> g)
                  ye <- delay(0.0) -< yd
                  yout <- ye.zip(yd).map { case (ye, yd) => (ye + yd) / 2 }.arrowBuilder
                } yield yout.zip(yout)
              } -< value.zip(g.zip(resonance))
            } yield y
          }
        }

        ArrowBuilder.build[SignalFunction, RealtimeEvents, Double] { in =>
          for {
            s1 <- nk.group1.slider.range(0.0, 1.0) -< in
            k1 <- nk.group1.knob.rangeExp(200, 10000) -< in
            s2 <- nk.group2.slider.range(0.0, 1000.0) -< in
            k2 <- nk.group2.knob.rangeExp(0.1, 100) -< in
            k3 <- nk.group3.knob.range(0.0, 10000.0) -< in
            s3 <- nk.group3.slider.range(0.0, 1.0) -< in
            masterVol <- nk.group8.slider.range(0.0, 1.0) -< in
            selector <- for {
              b0 <- nk.transport.rew.onPush -< in
              b1 <- nk.transport.ff.onPush -< in
              b2 <- nk.transport.stop.onPush -< in
              b3 <- nk.transport.play.onPush -< in
              b4 <- nk.transport.rec.onPush -< in
              num <- index(b0, b1, b2, b3, b4)(_.nonEmpty)
              ret <- continuous(0) -< num
            } yield ret
            lfo1 <- VCO.sin -< k2
            vco1 <- selectArrow(VCO.sin, VCO.saw) -< (selector -> (k1 + lfo1 * s2))
            vcf1 <- lfo(100) -< k3.zip(s3.zip(vco1))
            master <- vcf1.arrowBuilder
            out <- clip(-1.0, 1.0) -< master * masterVol
          } yield out
        }
      }
    } finally {
      nano.foreach { _.close() }
    }
  }
}
