import com.todesking.scalapp.syntax._
import scalaz.Arrow
import scalaz.syntax.arrow._
import scala.specialized

object Foo {
  def main(args: Array[String]): Unit = {
    import javax.sound.sampled._

    val format = new AudioFormat(44100, 8, 1, true, true)
    val line = AudioSystem.getSourceDataLine(format)
    line.open(format, 44100)
    line.start()

    val buf = new Array[Byte](44100).map(_ => 127.toByte)

    while (true) {
      line.write(buf, 0, buf.length)
    }

    line.drain();
    line.close();
  }
}

trait Simplify[+A, -B] {
  def bloat(b: B): A
}

object Simplify {
  implicit val evUnit0: Simplify[Unit, Unit] = new Simplify[Unit, Unit] {
    override def bloat(u: Unit): Unit = u
  }

  implicit def evUnitTuple2[A, B](implicit ev1: Simplify[A, Unit], ev2: Simplify[B, Unit]): Simplify[(A, B), Unit] =
    new Simplify[(A, B), Unit] {
      override def bloat(u: Unit): (A, B) = (ev1.bloat(u) -> ev2.bloat(u))
    }
}

sealed trait SignalFunction[-A, +B] {
  def buildProc(): A => B

  def shrink()(implicit ev: Simplify[A, Unit]): SignalFunction[Unit, B] =
    SignalFunction.const(()).mapsnd { u => ev.bloat(u) } >>> this

  def merge[C, D, E](rhs: SignalFunction[C, D])(f: (B, D) => E): SignalFunction[(A, C), E] =
    (this *** rhs).mapsnd(f.tupled)

  def +[C, D](rhs: SignalFunction[C, D])(implicit num: Numeric[D], evB: B <:< D): SignalFunction[(A, C), D] =
    this.merge(rhs) { (d1, d2) => num.plus(evB(d1), d2) }

  def *[C, D](rhs: SignalFunction[C, D])(implicit num: Numeric[D], evB: B <:< D): SignalFunction[(A, C), D] =
    this.merge(rhs) { (d1, d2) => num.times(evB(d1), d2) }
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

  implicit def sndInt2Double[A](sf: SignalFunction[A, Int]): SignalFunction[A, Double] =
    sf >>> SignalFunction(_.toDouble)

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

  val unit: ConstSF[Unit] = ConstSF(())

  def const[A](value: A): ConstSF[A] = ConstSF(value)

  def ignore[A]: SignalFunction[A, Unit] =
    SignalFunction { _ => () }
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

trait RealtimeEvent

sealed trait MidiEvent extends RealtimeEvent
case class MidiShortMessageEvent(command: Int, channel: Int, data1: Int, data2: Int) extends MidiEvent

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

  private[this] def pollEvent(t0: Long, samples: Long): Seq[RealtimeEvent] = {
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

  def runRealtime[A](genSF: SamplingRate => SignalFunction[A, Double])(implicit simplify: Simplify[A, Seq[RealtimeEvent]]): Unit =
    run(SignalFunction(simplify.bloat) >>> genSF(samplingRate))

  def runUnit[A](genSF: SamplingRate => SignalFunction[A, Double])(implicit simplify: Simplify[A, Unit]): Unit =
    run(SignalFunction.ignore[Seq[RealtimeEvent]] >>> SignalFunction(simplify.bloat) >>> genSF(samplingRate))

  private[this] def run(sf: SignalFunction[Seq[RealtimeEvent], Double]): Unit = {
    println(s"Running ${sf}...")
    val proc = sf.buildProc()
    val (line, bufSize) = JavaSound.openLine(samplingRate.value, 20000)
    val buffer = new Array[Byte](samplingRate.value * 10)

    try {
      val t0 = System.nanoTime()
      var clock: Long = 0L
      while (true) {
        for (i <- 0 until buffer.size) {
          // val firedEvents = pollEvent(t0, clock)
          val out = 0.0 // proc(firedEvents)
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
  import javax.sound.midi.ShortMessage
  class Button(cc: Int) {
    def unapply(m: ShortMessage): Option[Boolean] = ???
  }
  class Volume(cc: Int) {
    def unapply(m: ShortMessage): Option[Int] = ???
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
    // val Some(nano) = JavaSound.findNanoKontrol2()

    // nano.onMessage {
    //   case sm: javax.sound.midi.ShortMessage =>
    //     w.sendEvent(MidiShortMessageEvent(sm.getCommand, sm.getChannel, sm.getData1, sm.getData2))
    //   case m =>
    // }

    import SignalFunction.{ const }

    try {
      // nano.open()
      val eventSink = w.runUnit { implicit sr =>
        // (const(440.0) + (const(2.0) >>> VCO.sin) * const(20.0)) >>> VCO.saw
        const(440.0) >>> VCO.sin
      }
    } finally {
      // nano.close()
    }
  }
}
