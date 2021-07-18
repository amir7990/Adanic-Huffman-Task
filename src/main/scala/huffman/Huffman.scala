package huffman

import scala.:+

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: Vector[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
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

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with Vectors of characters. This function allows
   * you to easily create a character Vector from a given string.
   */
  def string2Chars(str: String): Vector[Char] = str.toVector

  /**
   * This function computes for each unique character in the Vector `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(Vector('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting Vector is not important):
   *
   *   Vector(('a', 2), ('b', 1))
   *
   * The type `Vector[(Char, Int)]` denotes a Vector of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
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

  /**
   * Returns a Vector of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned Vector should be ordered by ascending weights (i.e. the
   * head of the Vector should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafVector(freqs: Vector[(Char, Int)]): Vector[Leaf] = 
    freqs.sortWith((a, b) => a._2 < b._2).map(f => Leaf(f._1, f._2))

  /**
   * Checks whether the Vector `trees` contains only one single code tree.
   */
  def singleton(trees: Vector[CodeTree]): Boolean =
    if (trees.size == 1) true else false

  /**
   * The parameter `trees` of this function is a Vector of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the Vector `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a Vector of less than two elements, that Vector should be returned
   * unchanged.
   */
//  def combine(trees: Vector[CodeTree]): Vector[CodeTree] = trees match {
//    case left +: right +: t => (makeCodeTree(left, right) +: t).sortWith((t1,t2) => weight(t1) < weight(t2))
//    case _ => trees
//  }

  def combine(trees : Vector[CodeTree]): Vector[CodeTree] =
    if (trees.size <= 1) {
      trees
    }
    else {
      (makeCodeTree(trees.head, trees.tail.head) +: trees.tail.tail).sortWith((t1, t2) => weight(t1) < weight(t2))
    }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `Vector[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the Vector of
   * code trees contains only one single tree, and then return that singleton Vector.
   */
  def until(done: Vector[CodeTree] => Boolean, merge: Vector[CodeTree] => Vector[CodeTree])(trees: Vector[CodeTree]): Vector[CodeTree] =
    if (done(trees)) trees
    else {
      until(done, merge)(merge(trees))
    }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: Vector[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafVector( times(chars) ) ).head
  }


  // Part 3: Decoding

  type Bit = Byte

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting Vector of characters.
   */
  def decode(tree: CodeTree, b: Vector[Bit]): Vector[Char] = {
    def decode0(t: CodeTree, b: Vector[Bit]): Vector[Char] = t match {
    case Leaf(char, weight)  if (b.isEmpty) => Vector(char)
    case Leaf(char, weight) => char +: decode0(tree, b)
    case Fork(left, right, chars, weight)  if !b.isEmpty && b.head == 0.toByte => decode0(left, b.tail)
    case Fork(left, right, chars, weight)  => decode0(right, b.tail)
    }
    decode0(tree, b)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),Vector('x','j'),14279),Leaf('f',16351),Vector('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),Vector('k','w'),2492),Vector('z','k','w'),4585),Leaf('y',4725),Vector('z','k','w','y'),9310),Leaf('h',11298),Vector('z','k','w','y','h'),20608),Leaf('q',20889),Vector('z','k','w','y','h','q'),41497),Vector('x','j','f','z','k','w','y','h','q'),72127),Vector('d','x','j','f','z','k','w','y','h','q'),128396),Vector('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),Vector('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),Vector('m','p'),91856),Leaf('u',96785),Vector('m','p','u'),188641),Vector('o','l','m','p','u'),355071),Vector('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),Vector('g','b'),27110),Vector('v','g','b'),52085),Vector('c','v','g','b'),102088),Vector('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),Vector('n','t'),219915),Vector('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),Vector('i','a'),232575),Vector('e','i','a'),458522),Vector('r','c','v','g','b','n','t','e','i','a'),881025),Vector('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  def convertIntToByte(vector: Vector[Int]): Vector[Bit] =
    for {
      x <- vector
    } yield x.toByte


  val secret: Vector[Bit] = convertIntToByte(Vector(0,0,1,1,1,0,1,0,1,1,1,0,0,
    1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1))

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: Vector[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
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
