package nostrrent

import scala.annotation.tailrec

object Toolbox:

  def byteValue(char: Char): Int =
    if char >= '0' && char <= '9' then (char - '0')
    else if (char >= 'a' && char <= 'f') then (char - ('a' - 10))
    else if (char >= 'A' && char <= 'F') then (char - ('A' - 10))
    else throwIAE(s"Invalid hex char: $char")

  def hexToByteArray(hex: String): Array[Byte] =
    require(hex.length % 2 == 0, s"Must be byte aligned hexadecimal, was ${hex.length} hex chars")

    @tailrec
    def hexToBytes(i: Int = 0, out: Array[Byte] = new Array(hex.length / 2)): Array[Byte] =
      if i >= out.length then out
      else
        val b1 = byteValue(hex.charAt(i*2)) * 16
        val b2 = byteValue(hex.charAt(i*2+1))
        out(i) = (b1 + b2).asInstanceOf[Byte]
        hexToBytes(i+1, out)

    hexToBytes()
