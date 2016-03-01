import java.nio.file.{Files, Paths}

/**
 * Encodes and decodes text into wave files. The sound file to encode over the text must be at least
 * [[Steganography.BytesPerSample]]*[[Steganography.BitsInByte]] times larger in size than the text file or encoding
 * will fail. A more sophisticated implementation could read the bit-depth of the wave file, but here the bit-depth
 * is hard-coded.
 */
object Steganography {
  /**
   * Number of bytes in standard WAV file before sample data begins
   */
  val WaveHeaderSize = 44

  /**
   * Number of bits to reserve for encoding message length
   */
  val BitCountPrefixSize = 32

  /**
   * Number of bits in a byte; this should always be 8.
   */
  val BitsInByte = 8

  /**
   * Number of bits per wave sample; this must be a multiple of 8.
   */
  val WaveBitDepth = 16

  /**
   * Number of bytes per sample; 1 = 8-bit, 2 = 16-bit, etc. Calculated from WaveBitDepth.
   */
  val BytesPerSample = WaveBitDepth / BitsInByte

  /**
   * Encodes a text file into a specified wave file.
   *
   * @param soundPath Path to wave file containing sound to encode over.
   * @param textPath Path to text file to encode.
   * @param outputPath Path to write resulting encoded wave file to.
   */
  def encode(soundPath: String, textPath: String, outputPath: String): Unit = {
    // Read in sound and text files, grab bits to encode.
    val soundBytes = Files.readAllBytes(Paths.get(soundPath))
    val textBytes = Files.readAllBytes(Paths.get(textPath))
    val textBits = textBytes.flatMap(byte => toBits(BitsInByte)(byte.toInt))

    // Skip the first few bytes comprising the wave header, keep the header around as we'll
    // want to write it out the same way in our result file
    val (header, body) = soundBytes.splitAt(WaveHeaderSize)

    // Prepend the bit count header to indicate for decoders how long the message will be
    val messageBits = toBits(BitCountPrefixSize)(textBits.length) ++ textBits

    // Grab the section of the wave where we'll be writing our message, store the rest for later
    val (toEncode, skipped) = body.splitAt(messageBits.length*BytesPerSample)

    // Transform the samples, writing one bit of our message per sample - we overwrite the least
    // significant bit in each sample to minimize the audible effect
    val encoded = toEncode.grouped(BytesPerSample).zip(messageBits.iterator).flatMap {
      case (Array(first, rest @ _*), textBit) =>
        Array(if(textBit) toEven(first) else toOdd(first)) ++ rest
    }

    // Concatenate the header, our encoded message, and the rest of the samples together
    // and write the entire wave to the output path
    Files.write(Paths.get(outputPath), header ++ encoded ++ skipped)
    println(s"Successfully encoded to $outputPath")
  }

  /**
   * Decodes a text message from the specified wave file. This will fail dramatically if
   * the provided wave file didn't have a message encoded in it!
   *
   * @param soundPath Path to the sound file to decode.
   * @param outputPath Path to write the decoded message to.
   */
  def decode(soundPath: String, outputPath: String): Unit = {
    // Load the encoded wave to process
    val soundBytes = Files.readAllBytes(Paths.get(soundPath))

    // Skip the first few bytes comprising the wave header
    val body = soundBytes.drop(WaveHeaderSize)

    // Read the message length from the first set of encoded samples
    val messageLength = fromBits(decodeToBits(body.take(BitCountPrefixSize*BytesPerSample)))

    // Grab the encoded portion of the wave - the rest is irrelevant to us as it doesn't contain
    // any part of our message
    val toDecode = body.drop(BitCountPrefixSize*BytesPerSample).take(messageLength*BytesPerSample)

    // Transform the samples, pulling the encoded bit out from the least significant bit
    // of each sample
    val decoded = decodeToBits(toDecode).grouped(BitsInByte).map(fromBits).map(_.toByte).toArray

    // Write the resulting byte array, which should contain our decoded message, to the output path
    Files.write(Paths.get(outputPath), decoded)
    println(s"Successfully decoded to $outputPath")
  }

  /**
   * Helper function for decoding process. Takes a series of wave bytes and returns the encoded message bits
   * contained within it.
   *
   * @param bytes Wave bytes to decode.
   * @return Seq of decoded bits.
   */
  def decodeToBits(bytes: Array[Byte]) = {
    bytes.grouped(BytesPerSample).map {
      case Array(first, _*) => isEven(first)
    }.toSeq
  }

  /**
   * Converts an integer into a seq of its composite bits.
   *
   * @param numBits Number of bits to write.
   * @param n Integer to convert to bits.
   * @return Seq of the composite bits for this integer.
   */
  def toBits(numBits: Int)(n: Int) =
    0 until numBits map(bit => ((n >> bit) & 1) == 1)

  /**
   * Converts a seq of bits to an integer.
   *
   * @param bits Bits to convert.
   * @return The resulting integer.
   */
  def fromBits(bits: Seq[Boolean]) = bits.zipWithIndex.map {
    case (bit, i) => (if(bit) 1 else 0) << i
  }.sum

  /**
   * @param byte Byte to check.
   * @return True if the byte is even (smallest bit is 0).
   */
  def isEven(byte: Byte) = (byte & 1) == 0

  /**
   * Converts a byte to an even number by flipping the smallest bit appropriately.
   * This is equivalent to subtracting 1 if the number is odd.
   *
   * @param byte Byte to convert to even.
   * @return Converted byte.
   */
  def toEven(byte: Byte) = (byte & ~1).toByte

  /**
   * Converts a byte to an odd number by flipping the smallest bit appropriately.
   * This is equivalent to adding 1 if the number is even.
   *
   * @param byte Byte to convert to odd.
   * @return Converted byte.
   */
  def toOdd(byte: Byte) = (byte | 1).toByte
}
