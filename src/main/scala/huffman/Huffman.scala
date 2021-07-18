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


  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),Vector('x','j'),14279),Leaf('f',16351),Vector('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),Vector('k','w'),2492),Vector('z','k','w'),4585),Leaf('y',4725),Vector('z','k','w','y'),9310),Leaf('h',11298),Vector('z','k','w','y','h'),20608),Leaf('q',20889),Vector('z','k','w','y','h','q'),41497),Vector('x','j','f','z','k','w','y','h','q'),72127),Vector('d','x','j','f','z','k','w','y','h','q'),128396),Vector('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),Vector('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),Vector('m','p'),91856),Leaf('u',96785),Vector('m','p','u'),188641),Vector('o','l','m','p','u'),355071),Vector('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),Vector('g','b'),27110),Vector('v','g','b'),52085),Vector('c','v','g','b'),102088),Vector('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),Vector('n','t'),219915),Vector('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),Vector('i','a'),232575),Vector('e','i','a'),458522),Vector('r','c','v','g','b','n','t','e','i','a'),881025),Vector('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)


  def convertIntToByte(vector: Vector[Int]): Vector[Bit] =
    for {
      x <- vector
    } yield x.toByte


  val secret: Vector[Bit] = convertIntToByte(Vector(0,0,1,1,1,0,1,0,1,1,1,0,0,
    1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1))


  def decodedSecret: Vector[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree


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
