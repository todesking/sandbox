import javax.sound.sampled.{ AudioSystem, SourceDataLine, AudioFormat, DataLine }
import com.todesking.scalapp.syntax._

object Main {
  def main(args: Array[String]): Unit = {
    AudioSystem.getMixerInfo().foreach { mi =>
      val m = AudioSystem.getMixer(mi)
      print("Mixer: ")
      mi.pp()
      print("Source Lines: ")
      m.getSourceLineInfo().foreach(_.pp())
      print("Target Lines: ")
      m.getTargetLineInfo().foreach(_.pp())
    }

    val samples = 44100
    val channels = 1
    val sampleSize = 1
    val signed = true
    val bigEndian = false

    val bufferSizeMS = 50
    val bufferSizeByte = (samples * sampleSize * bufferSizeMS / 1000.0).toInt
    bufferSizeByte.pp()

    val format = new AudioFormat(samples.toFloat, sampleSize * 8, channels, signed, bigEndian)

    val info = new DataLine.Info(classOf[SourceDataLine], format)
    val line = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    line.open(format, bufferSizeByte)
    line.start()

    val buffer = new Array[Byte](bufferSizeByte)

    val hz = 800
    val step = 1.0 / samples

    def f(t: Double): Byte = {
      val a = (t * hz) % 1.0
      (-128 + a * 255).toByte
    }

    var t = 0.0

    while (true) {
      for (i <- 0 until bufferSizeByte) {
        buffer(i) = f(t)
        t += step
      }
      println(buffer.toSeq)
      line.write(buffer, 0, bufferSizeByte)
    }
  }
}
