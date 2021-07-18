package huffman
import Huffman._
class HuffmanClass(chars: Vector[Char]) {
  val tree = createCodeTree(chars)

  def Decode(bits: Vector[Bit]): String = {
    (decode(tree, bits)).mkString
  }

  def Encode(text: String): Vector[Bit] =
    encode(tree)(text.toVector)
}

