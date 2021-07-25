package huffman

import scala.annotation.tailrec

class Huffman {
  import CodeTree._

  type Bit = Byte

  def calDepth(tree : CodeTree, res: Int, c: Char): Int = {
    tree match {
      case Leaf(_,_) => res
      case Fork(left,_,_,_) if chars(left).contains(c) => calDepth(left, res+1, c)
      case Fork(_,right,_,_) => calDepth(right, res+1, c)
    }
  }

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
    def iterator(c: Vector[Char], frequency: Map[Char, Int]) : Map[Char, Int] = {
     if (c.isEmpty) {
       frequency
     } else {
       val count = frequency.getOrElse(c.head, 0) + 1
       iterator(c.tail, frequency + ((c.head, count)))
     }
    }
    iterator(chars, Map.empty).toVector
  }

  def makeOrderedLeafVector(freqs: Vector[(Char, Int)]): Vector[Leaf] =
    freqs.sortWith( _._2 < _._2).map(f => Leaf(f._1, f._2))

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

  def makeDiffer(shiftSize: Int): Int = 1 << shiftSize-1

  def decoder(tree: CodeTree, bits: (Int, Int)): Vector[Char] = {
    def decodeOneLeaf(t: CodeTree, b: (Int, Int)): Vector[Char] = {
      t match {
      case Leaf(char, _)  if b._2 == 0 => Vector(char)
      case Leaf(char, _) => char +: decodeOneLeaf(tree, b)
      case Fork(_, right, _, _)  if (b._1 & makeDiffer(b._2)) == makeDiffer(b._2) => decodeOneLeaf(right, (b._1, b._2-1))
      case Fork(left, _, _, _)  => decodeOneLeaf(left, (b._1, b._2-1))
      }
    }

    decodeOneLeaf(tree, bits)
  }

  def convertIntToByte(ints: Vector[Int]): Vector[Bit] =
    ints.map(a => a.toByte)

  def convertByteToBit(bits: Vector[Bit], ints: Vector[Int]): Vector[Int] = {
    def iterator(b: Vector[Bit], index: Int): Int = {
      if (b.isEmpty) {
        0
      } else {
        (b.head << index) | iterator(b.tail, index + 1)
      }
    }
    if(bits.isEmpty){
      ints
    }
    else{
      if (bits.size >= 32 ){
        val temp = bits.take(32)
        val int = iterator(temp.reverse, 0)
        convertByteToBit(bits.drop(32), ints:+int)
      }else{
        val temp = bits.take(bits.size)
        val int = iterator(temp.reverse, 0)
        convertByteToBit(bits.drop(bits.size), ints:+int)
      }
    }
  }

  def calcShiftSize(tree: CodeTree, chars: Vector[Char], res: Int): Int ={
    chars.foldRight(0)((a,b)=>{
      calDepth(tree, res, a)+b
    })
  }

  def encoder(tree: CodeTree)(text: Vector[Char]): (Int, Int) = {
    def encodeOneChar(tree: CodeTree, index: Int)(c: Char): Int= {
      tree match {
        case Leaf(_, _) => 0
        case Fork(_, right, _, _) if chars(right).contains(c) => 1 << index | encodeOneChar(right, index - 1)(c)
        case Fork(left, _, _, _) => 0 << index | encodeOneChar(left, index - 1)(c)
      }
    }
    @tailrec
    def iterator(chars: Vector[Char], res: Int, beatCount: Int): (Int, Int)= {
      if (chars.isEmpty){
        (res, beatCount)
      }else{
        val shiftSize = calcShiftSize(tree, chars.tail, 0)
        val depth = calDepth(tree, 0, chars.head)
        iterator(chars.tail, res|encodeOneChar(tree, depth-1)(chars.head)<<shiftSize, beatCount+depth)
      }
    }
    iterator(text, 0, 0)
  }
}
object Huffman extends Huffman
