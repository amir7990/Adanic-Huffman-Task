package huffman

import Huffman._

class HuffmanClass(chars: String) {
  
  val tree: CodeTree = createCodeTree(chars.toVector)

  def mainDecode(bits: Vector[Bit]): String = {
    decode(tree, bits).mkString
  }

  def mainEncode(text: String): Vector[Bit] = {
    encode(tree)(text.toVector)
  }
}

object HuffmanClass {
  def apply(chars: String): HuffmanClass = new HuffmanClass(chars)
}

<<<<<<< HEAD
=======
object HuffmanClass{
  def apply(chars: String): HuffmanClass = new HuffmanClass(chars)
}
>>>>>>> b0865f42a8aba4caa46004dbfb6c1214818f96c8

