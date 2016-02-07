import org.scalatest.FunSuite
import scala.io.Source

class CaslParserTest extends FunSuite {
  test("api-count1") {
    testCaslParser("ipa/count1.cas")
  }

  test("api-extern") {
    testCaslParser("ipa/extern.cas")
  }

  test("ipa-gsub2") {
    testCaslParser("ipa/gsub2.cas")
  }

  test("ipa-sample1") {
    testCaslParser("ipa/sample1.cas")
  }

  test("ipa-sample2") {
    testCaslParser("ipa/sample2.cas")
  }

  test("ipa-sample3") {
    testCaslParser("ipa/sample3.cas")
  }

  test("ipa-sample4") {
    testCaslParser("ipa/sample4.cas")
  }

  test("ipa-smain") {
    testCaslParser("ipa/smain.cas")
  }

  def testCaslParser(filename: String) {
    val source = Source.fromURL(getClass.getResource(filename), "Shift_JIS").mkString
    CaslParser.apply(source)
  }
}
