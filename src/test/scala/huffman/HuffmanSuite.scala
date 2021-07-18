package huffman

class HuffmanSuite extends munit.FunSuite {
  import Huffman._
  trait TestTrees1 {
    val huffman = new HuffmanClass("adddddbbbbcccc".toVector)
  }

  trait TestTrees2 {
    val huffman = new HuffmanClass("absdfdfdssaa".toVector)
  }

  test("First Test"){
    new TestTrees1  {
      assertEquals(huffman.Decode(huffman.Encode("aaabbcd")), "aaabbcd")
    }
  }

  test("Second Test"){
    new TestTrees1  {
      assertEquals(huffman.Decode(huffman.Encode("aaaddbbbbbbbb")), "aaaddbbbbbbbb")
    }
  }


  test("Third Test"){
    new TestTrees2  {
      assertEquals(huffman.Encode(huffman.Decode(convertIntToByte(Vector(1,0,1,0,1,0,1,1,1)))), convertIntToByte(Vector(1,0,1,0,1,0,1,1,1)))
    }
  }







}