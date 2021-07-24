package huffman

import Huffman._

class HuffmanClass(chars: String) {
  val tree: CodeTree = createCodeTree(chars.toVector)

  def mainDecode(bits: Int): String = {
    decoder(tree, bits).mkString
  }

  def mainEncode(text: String): Int =
    encoder(tree)(text.toVector)
}

object HuffmanClass{
  def apply(chars: String): HuffmanClass = new HuffmanClass(chars)
}

