package huffman
import Huffman._
class HuffmanClass(chars: Vector[Char]) {
  val tree = createCodeTree(chars)

  def Decode(bits: Vector[Bit]): Vector[Char] =
    decode(tree, bits)

  def Encode(text: Vector[Char]): Vector[Bit] =
    encode(tree)(text)
}

