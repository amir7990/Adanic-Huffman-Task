package huffman

import scala.:+


abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: Vector[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree


trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, w) => w
    case Fork(left, right, chars, weight) => weight
  }

  def chars(tree: CodeTree): Vector[Char] = tree match {
    case Leaf(c, _) => Vector(c)
    case Fork(_, _, cc, _) => cc
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left)++chars(right), weight(left) + weight(right))


  def string2Chars(str: String): Vector[Char] = str.toVector


  def times(chars: Vector[Char]): Vector[(Char, Int)] = {
    def iter(ch: Vector[Char], v: Map[Char, Int]) : Map[Char, Int] = {
     if (ch.isEmpty)
       v
     else {
       val count = v.getOrElse(ch.head, 0) + 1
       iter(ch.tail, v + ((ch.head, count)))
     }
    }
    iter(chars, Map()).toVector
  }


  def makeOrderedLeafVector(freqs: Vector[(Char, Int)]): Vector[Leaf] =
    freqs.sortWith((a, b) => a._2 < b._2).map(f => Leaf(f._1, f._2))


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
    if (done(trees)) trees
    else {
      until(done, merge)(merge(trees))
    }


  def createCodeTree(chars: Vector[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafVector( times(chars) ) ).head
  }




  type Bit = Byte


  def decode(tree: CodeTree, b: Vector[Bit]): Vector[Char] = {
    def decode0(t: CodeTree, b: Vector[Bit]): Vector[Char] = t match {
    case Leaf(char, weight)  if (b.isEmpty) => Vector(char)
    case Leaf(char, weight) => char +: decode0(tree, b)
    case Fork(left, right, chars, weight)  if !b.isEmpty && b.head == 0.toByte => decode0(left, b.tail)
    case Fork(left, right, chars, weight)  => decode0(right, b.tail)
    }
    decode0(tree, b)
  }


  def convertIntToByte(vector: Vector[Int]): Vector[Bit] =
    for {
      x <- vector
    } yield x.toByte


  def encode(tree: CodeTree)(text: Vector[Char]): Vector[Bit] = {
    def encode0(tree: CodeTree)(c: Char): Vector[Bit] = tree match {
    case Leaf(char, weight) => Vector()
    case Fork(left, right, _, _) if chars(left).contains(c) => 0.toByte +: encode0(left)(c)
    case Fork(left, right, _, _) => 1.toByte +: encode0(right)(c)
    }
    text flatMap encode0(tree)
  }



}
object Huffman extends Huffman
