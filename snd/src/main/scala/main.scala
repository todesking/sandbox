import com.todesking.scalapp.syntax._
import scalaz.Arrow
import scalaz.syntax.arrow._
import scala.specialized

sealed trait SignalFunction[-A, +B] {
  def buildProc(): A => B
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

  def apply[A, B](f: A => B): FunctionSF[A, B] =
    FunctionSF[A, B](f)

  def stateful[A, B, @specialized(Int, Double) C](init: C)(f: (A, C) => (B, C)): StatefulSF[A, B, C] =
    StatefulSF(FunctionSF[(A, C), (B, C)](_ match { case (a, c) => f(a, c) }), init)

  val ignore: ConstSF[Unit] = ConstSF(())

  def const[A](value: A): ConstSF[A] = ConstSF(value)
}

sealed trait MidiEvent
case class MidiShortMessage(command: Int, channel: Int, data1: Int, data2: Int) extends MidiEvent

case class SamplingRate(value: Int) extends AnyVal

object VCO {
  def saw(implicit sr: SamplingRate) = SignalFunction.stateful[Double, Double, Double](0.0) { (freq, state) =>
    val phase = (state + freq / sr.value) % 1.0
    val value = phase * 2 - 1.0
    (value -> phase)
  }
}

class World(samplingRate: SamplingRate) {
  def run[A](init: A)(genSF: SamplingRate => SignalFunction[A, Double]): Unit = {
    val sf = genSF(samplingRate)
    println(s"Running ${sf}...")
    run(init, sf)
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
}

object Main {
  def main(args: Array[String]): Unit = {
    val w = World.create(SamplingRate(44100))
    w.run[Unit](()) { implicit sr =>
      SignalFunction.const(440.0) >>> VCO.saw
    }
  }
}
