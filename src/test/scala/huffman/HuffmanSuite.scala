package huffman

class HuffmanSuite extends munit.FunSuite {
  import Huffman._
  trait TestTrees {
    val huffman = new HuffmanClass("adddddbbbbcccc".toVector)
  }
  
  test("1"){
    new TestTrees  {
      assertEquals(huffman.Decode(huffman.Encode("aaabbcd".toVector)), "aaabbcd".toVector)
    }
  }

  test("2"){
    new TestTrees  {
      assertEquals(huffman.Decode(huffman.Encode("aaaddbbbbbbbb".toVector)), "aaaddbbbbbbbb".toVector)
    }
  }







}