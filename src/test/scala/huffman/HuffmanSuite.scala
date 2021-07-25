package huffman

class HuffmanSuite extends munit.FunSuite {
  test("test1") {
    val string = " < 32bit"
    val huffman = HuffmanClass(string)
    val encoded = huffman.mainEncode(string)
    val decoded = huffman.mainDecode(encoded)
    assertEquals(decoded, string)
  }
}

