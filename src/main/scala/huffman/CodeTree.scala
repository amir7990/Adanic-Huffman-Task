package huffman

abstract class CodeTree


object CodeTree extends CodeTree{

  case class Fork(left: CodeTree, right: CodeTree, chars: Vector[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

}