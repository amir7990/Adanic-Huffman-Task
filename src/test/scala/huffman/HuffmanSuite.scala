package huffman

class HuffmanSuite extends munit.FunSuite {
  import Huffman._

  trait TestTrees1 {
    val huffman: HuffmanClass = HuffmanClass("adddddbbbbcccc")
  }

  trait TestTrees2 {
    val huffman: HuffmanClass = HuffmanClass("absdfdfdssaa")
  }

  test("First Test"){
    new TestTrees1  {
      assertEquals(huffman.mainDecode(huffman.mainEncode("aaabbcd")), "aaabbcd")
    }
  }

  test("Second Test"){
    new TestTrees1  {
      assertEquals(huffman.mainDecode(huffman.mainEncode("aaaddbbbbbbbb")), "aaaddbbbbbbbb")
    }
  }

  test("Third Test"){
    new TestTrees2  {
      assertEquals(huffman.mainEncode(huffman.mainDecode(convertIntToByte(Vector(1,0,1,0,1,0,1,1,1)))), convertIntToByte(Vector(1,0,1,0,1,0,1,1,1)))
    }
  }







}