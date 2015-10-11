import com.todesking.scalapp.syntax._
import scalaz.Arrow
import scalaz.syntax.arrow._
import scala.specialized

trait RealtimeEvent

object hack {
  type RealtimeEvents = Seq[RealtimeEvent]
}
import hack._


object ArrowSyntax {
  implicit class ArrowExt[F[_, _]: Arrow, A, B](self: F[A, B]) {
    def -<[C](s: Signal[F, A]): ArrowBuilder[F, C, A, B] =
      ArrowBuilder.Bind[F, C, A, B](self, s)
  }

  implicit def buildArrow[F[_, _], A, B](ab: ArrowBuilder[F, A, A, B]): F[A, B] =
    ab.build()
}

// A: root of arrow
// B: start point of arrow that currently building
// C: destination of arrow
trait ArrowBuilder[F[_, _], A, +B, C] {
  final val dest: Signal[F, C] =
    Signal.Dest(this)

  def map[D](f: Signal[F, C] => Signal[F, D]): ArrowBuilder[F, A, B, D] =
    ArrowBuilder.Map[F, A, B, C, D](this, f)

  def flatMap[D, E](f: Signal[F, C] => ArrowBuilder[F, A, D, E]): ArrowBuilder[F, A, D, E] =
    ArrowBuilder.FlatMap[F, A, B, C, D, E](this, f)

  def build(): F[A, C] = ???
}

object ArrowBuilder {
  def build[F[_, _], A, B](f: Signal[F, A] => ArrowBuilder[F, A, Any, B]): F[A, B] =
    Start[F, A]().flatMap(f).build()

  case class Start[F[_, _], A]() extends ArrowBuilder[F, A, A, A]
  case class Map[F[_, _], A, B, C, D](ab: ArrowBuilder[F, A, B, C], f: Signal[F, C] => Signal[F, D]) extends ArrowBuilder[F, A, B, D]
  case class FlatMap[F[_, _], A, B, C, D, E](ab: ArrowBuilder[F, A, B, C], f: Signal[F, C] => ArrowBuilder[F, A, D, E]) extends ArrowBuilder[F, A, D, E]
  case class Bind[F[_, _], A, B, C](ab: F[B, C], s: Signal[F, B]) extends ArrowBuilder[F, A, B, C]
}

trait Signal[F[_, _], A] {
  def map[B](f: A => B): Signal[F, B] =
    Signal.Map[F, A, B](this, f)

  def zip[B](b: Signal[F, B]): Signal[F, (A, B)] =
    Signal.Zip[F, A, B](this, b)
}

object Signal {
  case class Dest[F[_, _], A](ab: ArrowBuilder[F, _, _, A]) extends Signal[F, A]
  case class Map[F[_, _], A, B](s: Signal[F, A], f: A => B) extends Signal[F, B]
  case class Zip[F[_, _], A, B](a: Signal[F, A], b: Signal[F, B]) extends Signal[F, (A, B)]
}

trait Compat[A, B] {
  def bloat(a: A): B
  def shrink(b: B): A
}

class Not[A, B]

object Not {
  implicit def ev[A, B] = new Not[A, B]
  implicit def evAmb[A, B](implicit e: A =:= B): Not[A, B] = sys.error("aaagghh")
}

object Compat {
  private[this] def gen[A, B](f: A => B)(g: B => A): Compat[A, B] =
    new Compat[A, B] { override def bloat(a: A): B = f(a); override def shrink(b: B): A = g(b) }

  implicit def evId[A]: Compat[A, A] =
    gen[A, A](identity)(identity)

  implicit def evTuple2Unit: Compat[Unit, (Unit, Unit)] =
    gen[Unit, (Unit, Unit)] { u => (() -> ()) } { case (_, _) => () }

  implicit def evTuple2R[A, B](implicit ev1: Compat[A, B], nu: Not[B, Unit]): Compat[A, (Unit, B)] =
    gen[A, (Unit, B)] { a => (() -> ev1.bloat(a)) } { case (u, b) => ev1.shrink(b) }

  implicit def evTuple2L[A, B](implicit ev1: Compat[A, B], nu: Not[B, Unit]): Compat[A, (B, Unit)] =
    gen[A, (B, Unit)] { a => (ev1.bloat(a), ()) } { case (b, u) => ev1.shrink(b) }
}

sealed trait SignalFunction[-A, +B] {
  def buildProc(): A => B

  def merge[C, D, E](rhs: SignalFunction[C, D])(f: (B, D) => E): SignalFunction[(A, C), E] =
    (this *** rhs).mapsnd(f.tupled)

  def +[C, D](rhs: SignalFunction[C, D])(implicit num: Numeric[D], evB: B <:< D): SignalFunction[(A, C), D] =
    this.merge(rhs) { (d1, d2) => num.plus(evB(d1), d2) }

  def *[C, D](rhs: SignalFunction[C, D])(implicit num: Numeric[D], evB: B <:< D): SignalFunction[(A, C), D] =
    this.merge(rhs) { (d1, d2) => num.times(evB(d1), d2) }

  def >>>~<[C, D](rhs: SignalFunction[C, D])(implicit ev: Compat[_ >: B, C]): SignalFunction[A, D] =
    this >>> SignalFunction(ev.bloat) >>> rhs

  def >>>~>[C, D](rhs: SignalFunction[C, D])(implicit ev: Compat[C, _ >: B]): SignalFunction[A, D] =
    this >>> SignalFunction(ev.shrink) >>> rhs
}

case class FunctionSF[A, B](f: A => B) extends SignalFunction[A, B] {
  override def buildProc(): A => B = f
}
case class IdSF[A]() extends SignalFunction[A, A] {
  override def buildProc(): A => A = identity
}
case class BypassSF[A, B, C](sf: SignalFunction[A, B]) extends SignalFunction[(A, C), (B, C)] {
  override def buildProc(): ((A, C)) => (B, C) = {
    val proc = sf.buildProc()
    ac: (A, C) => (proc(ac._1) -> ac._2)
  }
}
case class ComposeSF[A, B, C](sfBC: SignalFunction[B, C], sfAB: SignalFunction[A, B]) extends SignalFunction[A, C] {
  override def buildProc(): A => C =
    sfAB.buildProc() andThen sfBC.buildProc()
}
case class StatefulSF[A, B, C](sf: SignalFunction[(A, C), (B, C)], init: C) extends SignalFunction[A, B] {
  override def buildProc(): A => B = {
    val proc = sf.buildProc()
    var i = init

    a => {
      val (b, ii) = proc(a -> i)
      i = ii
      b
    }
  }
}
case class ConstSF[@specialized(Int, Double) A](value: A) extends SignalFunction[Unit, A] {
  override def buildProc(): Unit => A =
    _ => value
}

object SignalFunction {
  implicit def arrowInstance[A, B]: Arrow[SignalFunction] = new Arrow[SignalFunction] {
    override def arr[A, B](f: A => B): SignalFunction[A, B] =
      FunctionSF(f)
    override def id[A]: SignalFunction[A, A] =
      IdSF[A]()
    override def first[A, B, C](sf: SignalFunction[A, B]): SignalFunction[(A, C), (B, C)] =
      BypassSF[A, B, C](sf)
    override def compose[A, B, C](sf1: SignalFunction[B, C], sf2: SignalFunction[A, B]): SignalFunction[A, C] =
      ComposeSF[A, B, C](sf1, sf2)
  }

  implicit def fit[A, B, C, D](sf: SignalFunction[A, B])(implicit evFst: Compat[C, A], evSnd: Compat[D, B]): SignalFunction[C, D] =
    id[C] >>>~< sf >>>~> id[D]

  implicit def autoConst[A](a: A): SignalFunction[Unit, A] =
    const(a)

  def apply[A, B](f: A => B): SignalFunction[A, B] =
    FunctionSF[A, B](f)

  def stateful[A, B, @specialized(Int, Long, Double) C](init: C)(f: (A, C) => (B, C)): StatefulSF[A, B, C] =
    StatefulSF(FunctionSF[(A, C), (B, C)](_ match { case (a, c) => f(a, c) }), init)

  def stateOut[A, @specialized(Int, Long, Double) B](init: B)(f: (A, B) => B): StatefulSF[A, B, B] =
    stateful[A, B, B](init) {
      case (a, b) =>
        val b_ = f(a, b)
        (b_ -> b_)
    }

  def id[A]: SignalFunction[A, A] = IdSF[A]

  val unit: ConstSF[Unit] = ConstSF(())

  def const[A](value: A): ConstSF[A] = ConstSF(value)

  def ignore[A]: SignalFunction[A, Unit] =
    SignalFunction { _ => () }

  def extractEvent[A](pf: PartialFunction[RealtimeEvent, A]): SignalFunction[RealtimeEvents, Seq[A]] =
    SignalFunction(_.collect(pf))

  def expscale(max: Double): SignalFunction[Double, Double] =
    SignalFunction { v => math.pow(max, v) }

  def zeroone[A]: SignalFunction[Seq[A], Option[A]] =
    SignalFunction(_.headOption)

  def continuous[A](init: A): SignalFunction[Option[A], A] =
    stateOut(init) {
      case (Some(ev), st) => ev
      case (None, st) => st
    }
}

case class SamplingRate(value: Int) extends AnyVal

object Primitive {
  // NOTE: phase is normalized(0.0 to 1.0)
  def freqToPhase(implicit sr: SamplingRate): SignalFunction[Double, Double] =
    SignalFunction.stateOut[Double, Double](0.0) { (freq, state) => (state + freq / sr.value) % 1.0 }
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
    return Seq.empty
    val clockNanos = (samples.toDouble / (samplingRate.value * 1000L * 1000L * 1000L)).toLong
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

  def runRealtime[A](genSF: SamplingRate => SignalFunction[A, Double])(implicit ev: Compat[RealtimeEvents, A]): Unit =
    run(SignalFunction(ev.bloat) >>> genSF(samplingRate))

  def runUnit[A](genSF: SamplingRate => SignalFunction[A, Double])(implicit ev: Compat[Unit, A]): Unit =
    run(SignalFunction.ignore[RealtimeEvents] >>> SignalFunction(ev.bloat) >>> genSF(samplingRate))

  private[this] def run(sf: SignalFunction[RealtimeEvents, Double]): Unit = {
    println(s"Running ${sf}...")
    val proc = sf.buildProc()
    val (line, bufSize) = JavaSound.openLine(samplingRate.value, 20000)
    val buffer = new Array[Byte](samplingRate.value * 10)

    try {
      val t0 = System.nanoTime()
      var clock: Long = 0L
      while (true) {
        for (i <- 0 until buffer.size) {
          val firedEvents = pollEvent(t0, clock)
          val out = proc(firedEvents)
          buffer(i) = (out * 127).toByte
          clock += 1
        }
        line.write(buffer, 0, buffer.size)
        println(s"${line.available()}/${line.getBufferSize}")
      }
    } finally {
      line.close()
    }
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

    val format = new AudioFormat(samplingRate.toFloat, sampleSize * 8, channels, signed, bigEndian)

    val info = new DataLine.Info(classOf[SourceDataLine], format, bufferSizeByte)
    val line = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    line.open(format, bufferSizeByte)
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
  class Button(val cc: Int) {
    def toPF: PartialFunction[RealtimeEvent, Boolean] = {
      case MidiShortMessageEvent(cmd, ch, d1, d2) if cc == d1 =>
        d2 == 127
    }
  }
  class Volume(val cc: Int) {
    def toPF: PartialFunction[RealtimeEvent, Double] = {
      case MidiShortMessageEvent(cmd, ch, d1, d2) if cc == d1 =>
        math.min(math.max(d2, 0), 127) / 127.0
    }
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
    val w = new World(SamplingRate(44100), 100L * 1000 * 1000, 100L * 1000 * 1000)

    val nano = JavaSound.findNanoKontrol2()
    nano foreach {
      _.onMessage {
        case sm: javax.sound.midi.ShortMessage =>
          w.sendEvent(MidiShortMessageEvent(sm.getCommand, sm.getChannel, sm.getData1, sm.getData2))
        case m =>
      }
    }

    import SignalFunction.{ const, ignore, extractEvent, id, continuous, zeroone, expscale }
    import ArrowSyntax._
    type SF[A, B] = SignalFunction[A, B]
    try {
      nano foreach { _.open() }
      val eventSink = w.runRealtime { implicit sr =>
        val nk = NanoKontrol2

        def readVolume(
          extract: PartialFunction[RealtimeEvent, Double],
          max: Double
        ): SF[RealtimeEvents, Double] =
          extractEvent(extract) >>>
          zeroone[Double] >>>
          continuous(0.0) >>>
          SignalFunction(_ * max)

        def readVolumeExp(
          extract: PartialFunction[RealtimeEvent, Double],
          max: Double
        ): SF[RealtimeEvents, Double] =
          readVolume(extract, 1.0) >>> expscale(max)

        val ab = ArrowBuilder
        ArrowBuilder.build[SignalFunction, RealtimeEvents, Double] { in =>
          for {
            s1 <- readVolumeExp(nk.group1.slider.toPF, 10000) -< in
            k1 <- readVolumeExp(nk.group1.knob.toPF, 1.0) -< in
            w <- VCO.sin -< k1
          } yield w.zip(s1).map { case (a, b) => a * b }
        }
      }
    } finally {
      nano.foreach { _.close() }
    }
  }
}
