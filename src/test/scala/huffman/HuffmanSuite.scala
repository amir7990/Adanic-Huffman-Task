package huffman

class HuffmanSuite extends munit.FunSuite {
//  trait TestTrees1 {
//    val huffman: HuffmanClass = HuffmanClass("adddddbbbbcccc")
//  }
//
//  trait TestTrees2 {
//    val huffman: HuffmanClass = HuffmanClass("absdfdfdssaa")
//  }
//
//  test("First Test"){
//    new TestTrees1  {
//      assertEquals(huffman.mainDecode(huffman.mainEncode("aaabbcd")), "aaabbcd")
//    }
//  }
//
//  test("Second Test"){
//    new TestTrees1  {
//      assertEquals(huffman.mainDecode(huffman.mainEncode("aaaddbbbbbbbb")), "aaaddbbbbbbbb")
//    }
//  }
//
//  test("Third Test"){
//    new TestTrees2  {
//      assertEquals(huffman.mainEncode(huffman.mainDecode(convertIntToByte(Vector(1,0,1,0,1,0,1,1,1)))), convertIntToByte(Vector(1,0,1,0,1,0,1,1,1)))
//    }
//  }
  test("test") {
    val huffman = HuffmanClass("abccddd")
    val tree = huffman.tree
    val encoded = huffman.mainEncode("abccddd")
    val decoded = huffman.mainDecode(encoded)
    println(encoded, decoded, tree)
  }
}

