package ftanml.util

/**
 * Utility methods and data for Unicode manipulation, especially surrogate pairs
 */

object Unicode {

  val NONBMP_MIN: Int = 0x10000
  val NONBMP_MAX: Int = 0x10FFFF
  val SURROGATE1_MIN: Char = 0xD800
  val SURROGATE1_MAX: Char = 0xDBFF
  val SURROGATE2_MIN: Char = 0xDC00
  val SURROGATE2_MAX: Char = 0xDFFF

  /**
   * Convert a Unicode codepoint to a String, either a single-character string
   * in the case of a BMP codepoint, or a surrogate pair in the case of a non-BMP codepoint
   */

  def surrogatePair(code : Int) : String = {
    if (code <= 65535) {
      code.asInstanceOf[Char].toString
    } else {
      "" + highSurrogate(code) + lowSurrogate(code)
    }
  }

  def isHighSurrogate(ch: Char) = ch >= SURROGATE1_MIN && ch <= SURROGATE1_MAX

  def isLowSurrogate(ch: Char) = ch >= SURROGATE2_MIN && ch <= SURROGATE2_MAX

  /**
   * Return the high surrogate of a non-BMP character
   * @param ch The Unicode codepoint of the non-BMP character to be divided.
   * @return the first character in the surrogate pair
   */
  def highSurrogate(ch: Int): Char = {
    (((ch - NONBMP_MIN) >> 10) + SURROGATE1_MIN).asInstanceOf[Char]
  }

  /**
   * Return the low surrogate of a non-BMP character
   * @param ch The Unicode codepoint of the non-BMP character to be divided.
   * @return the second character in the surrogate pair
   */
  def lowSurrogate(ch: Int): Char = {
    (((ch - NONBMP_MIN) & 0x3FF) + SURROGATE2_MIN).asInstanceOf[Char]
  }

  /**
   * Return the non-BMP character corresponding to a given surrogate pair
   * surrogates.
   * @param high The high surrogate.
   * @param low The low surrogate.
   * @return the Unicode codepoint represented by the surrogate pair
   */
  def combinePair(high: Char, low: Char): Int = {
    (high - SURROGATE1_MIN) * 0x400 + (low - SURROGATE2_MIN) + NONBMP_MIN
  }

  /**
   * Convert a string to a sequence of integer codepoints
   */

  def codepoints(in: String):Seq[Int] = {
    for ((i : Int) <- 0 to in.length - 1;
         c = in.charAt(i)
         if (!isLowSurrogate(c))
    ) yield {
      if (isHighSurrogate(c)) {
         combinePair(c, in.charAt(i+1))
      } else {
         c
      }
    }
  }

}