import com.todesking.scalapp.syntax._
import scalaz.Arrow
import scalaz.syntax.arrow._
import scala.specialized

trait Simplify[+A, -B] {
  def build(b: B): A
}

object Simplify {
  implicit val evUnit0: Simplify[Unit, Unit] = new Simplify[Unit, Unit] {
    override def build(u: Unit): Unit = u
  }

  implicit def evUnitTuple2[A, B](implicit ev1: Simplify[A, Unit], ev2: Simplify[B, Unit]): Simplify[(A, B), Unit] =
    new Simplify[(A, B), Unit] {
      override def build(u: Unit): (A, B) = (ev1.build(u) -> ev2.build(u))
    }
}

sealed trait SignalFunction[-A, +B] {
  def buildProc(): A => B

  def shrink()(implicit ev: Simplify[A, Unit]): SignalFunction[Unit, B] =
    SignalFunction.const(()).mapsnd { u => ev.build(u) } >>> this

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

  def apply[A, B](f: A => B): FunctionSF[A, B] =
    FunctionSF[A, B](f)

  def stateful[A, B, @specialized(Int, Double) C](init: C)(f: (A, C) => (B, C)): StatefulSF[A, B, C] =
    StatefulSF(FunctionSF[(A, C), (B, C)](_ match { case (a, c) => f(a, c) }), init)

  val unit: ConstSF[Unit] = ConstSF(())

  def const[A](value: A): ConstSF[A] = ConstSF(value)
}

sealed trait MidiEvent
case class MidiShortMessage(command: Int, channel: Int, data1: Int, data2: Int) extends MidiEvent

case class SamplingRate(value: Int) extends AnyVal

object Primitive {
  // NOTE: phase is normalized(0.0 to 1.0)
  def freqToPhase(implicit sr: SamplingRate): SignalFunction[Double, Double] =
    SignalFunction.stateful[Double, Double, Double](0.0) { (freq, state) =>
      val phase = (state + freq / sr.value)
      (phase -> phase)
    }
}

object VCO {
  import Primitive._

  def saw(implicit sr: SamplingRate): SignalFunction[Double, Double] =
    freqToPhase >>> SignalFunction { phase => phase * 2 - 1.0 }

  def sin(implicit sr: SamplingRate): SignalFunction[Double, Double] =
    freqToPhase >>> SignalFunction { phase => math.sin(phase * 2 * math.Pi) }
}

class World(samplingRate: SamplingRate) {
  def runUnit[A](genSF: SamplingRate => SignalFunction[A, Double])(implicit simplify: Simplify[A, Unit]): Unit = {
    val sf = genSF(samplingRate)
    println(s"Running ${sf}...")
    run[Unit]((), SignalFunction.unit >>> SignalFunction(simplify.build) >>> sf)
  }

  private[this] def run[A](init: A, sf: SignalFunction[A, Double]): Unit = {
    val proc = sf.buildProc()
    val (line, bufSize) = JavaSound.openLine(samplingRate.value, 100)
    val buffer = new Array[Byte](bufSize)

    while (true) {
      for (i <- 0 until bufSize) {
        buffer(i) = (proc(init) * 255 - 128).toByte
      }
      line.write(buffer, 0, bufSize)
    }
  }
}

object World {
  def create(sr: SamplingRate): World =
    new World(sr)
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

    val bufferSizeMS = 50
    val bufferSizeByte = (samplingRate * sampleSize * bufferSizeMS / 1000.0).toInt

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

  def findNanoKontrol(): Option[(MidiDevice, MidiDevice)] = {
    val korg = "KORG INC."
    for {
      in <- findMidiDevice("SLIDER/KNOB", korg)
      out <- findMidiDevice("CTRL", korg)
    } yield (in -> out)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val w = World.create(SamplingRate(44100))
    // val Some((in, out)) = JavaSound.findNanoKontrol()

    import SignalFunction.{ const }

    w.runUnit { implicit sr =>
      (const(440.0) + (const(2.0) >>> VCO.sin) * const(20.0)) >>> VCO.saw
    }
  }
}
