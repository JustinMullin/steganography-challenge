

/**
 * Main entry point for command line execution. Passes valid commands off to
 * [[Steganography]] for encoding/decoding.
 */
object Contact extends App {
  args match {
    case Array("encode", soundPath, textPath, outputPath) =>
      Steganography.encode(soundPath, textPath, outputPath)
    case Array("decode", soundPath, outputPath) =>
      Steganography.decode(soundPath, outputPath)
    case _ =>
      println(
        """
          |usage: contact encode inputWavFile inputTxtFile outputWavFile
          |       contact decode inputWavFile outputTxtFile
        """.stripMargin)
  }
}
