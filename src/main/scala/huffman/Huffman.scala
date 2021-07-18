package huffman

import scala.annotation.tailrec

abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: Vector[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

trait Huffman {

  type Bit = Byte

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, w) => w
    case Fork(_, _, _, weight) => weight
  }

  def chars(tree: CodeTree): Vector[Char] = tree match {
    case Leaf(c, _) => Vector(c)
    case Fork(_, _, cc, _) => cc
  }

  def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree =
    Fork(left, right, chars(left)++chars(right), weight(left) + weight(right))

  def string2Chars(str: String): Vector[Char] = str.toVector

  def times(chars: Vector[Char]): Vector[(Char, Int)] = {
    @tailrec
    def iterator(ch: Vector[Char], v: Map[Char, Int]) : Map[Char, Int] = {
     if (ch.isEmpty) {
       v
     } else {
       val count = v.getOrElse(ch.head, 0) + 1
       iterator(ch.tail, v + ((ch.head, count)))
     }
    }
    iterator(chars, Map.empty).toVector
  }

  def makeOrderedLeafVector(freq: Vector[(Char, Int)]): Vector[Leaf] =
    freq.sortWith( _._2 < _._2).map(f => Leaf(f._1, f._2))

  def singleton(trees: Vector[CodeTree]): Boolean =
    if (trees.size == 1) true else false

  def combine(trees : Vector[CodeTree]): Vector[CodeTree] =
    if (trees.size <= 1) {
      trees
    }
    else {
      (makeCodeTree(trees.head, trees.tail.head) +: trees.tail.tail).sortWith((t1, t2) => weight(t1) < weight(t2))
    }

  def until(done: Vector[CodeTree] => Boolean, merge: Vector[CodeTree] => Vector[CodeTree])(trees: Vector[CodeTree]): Vector[CodeTree] =
    if (done(trees)) {
      trees
    } else {
      until(done, merge)(merge(trees))
    }

  def createCodeTree(chars: Vector[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafVector( times(chars) ) ).head
  }

  def decode(tree: CodeTree, b: Vector[Bit]): Vector[Char] = {
    def decode0(t: CodeTree, b: Vector[Bit]): Vector[Char] = t match {
      case Leaf(char, _)  if b.isEmpty => Vector(char)
      case Leaf(char, _) => char +: decode0(tree, b)
      case Fork(left, _, _, _)  if b.head == 0.toByte => decode0(left, b.tail)
      case Fork(_, right, _, _)  => decode0(right, b.tail)
    }
    decode0(tree, b)
  }

  def convertIntToByte(vector: Vector[Int]): Vector[Bit] =
    vector.map(a => a.toByte)

  def encode(tree: CodeTree)(text: Vector[Char]): Vector[Bit] = {
    def encode0(tree: CodeTree)(c: Char): Vector[Bit] = tree match {
      case Leaf(_, _) => Vector.empty
      case Fork(left, _, _, _) if chars(left).contains(c) => 0.toByte +: encode0(left)(c)
      case Fork(_, right, _, _) => 1.toByte +: encode0(right)(c)
    }
    text flatMap encode0(tree)
  }
}
object Huffman extends Huffman
