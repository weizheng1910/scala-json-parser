import org.scalatest.funsuite.AnyFunSuite

class TestLex extends AnyFunSuite {

  test("lex string") {
    val string = "\"hello\"1234"
    val ans = JsonLexer.lexString(List[Any](), string)
    assert(ans._1 == List("hello"))
    assert(ans._2 == "1234")
  }

  test("lex integer"){
    val string = "1234 \n \"party:\"yes\""
    val ans = JsonLexer.lexInteger(List[Any](), string)
    assert(ans._1 == List("1234"))

  }

  test("substring == 1"){
    val string = "a"
    val res = string.substring(1)
    assert(res.length == 0)
  }

  test("drop(1) should be like array.tail"){
    val array = Array(1,2,3)
    val result = array.drop(1)
    assert(result(0)== 2)
  }

  test("parseObject"){
    val array = List("key",":","value","}")
    val result = JsonParser.parseObject(array)
    assert(result != null)
  }

  test("test simple json object"){
    val jsonString = "{\n  \"fieldOne\" : 1,\n  \"fieldTwo\" : 2\n}"
    val firstStep = JsonLexer.lex(jsonString, List[Any]())
    assert(firstStep.length == 9)
  }

  test("test simple json object char array"){
    val jsonString = "{\n  \"fieldOne\" : 1,\n  \"fieldTwo\" : 2\n}"
    val toChar = jsonString.toCharArray
    val result = toChar
  }

  test("FINAL - simple json object - string and integer"){
    val jsonString = "{\n  \"fieldOne\" : 1,\n  \"fieldTwo\" : 2\n}"
    val firstStep = JsonLexer.lex(jsonString, List[Any]())
    val ans = JsonParser.parse(firstStep)
  }

  test("FINAL - simple json object - string and array"){
    val jsonString = "{\n  \"fieldOne\" : [1,2,3]\n  \"fieldTwo\" : \"hey\"\n}"
    val firstStep = JsonLexer.lex(jsonString, List[Any]())
    val ans = JsonParser.parse(firstStep)
    assert(ans.get("fieldOne").get == List("1","2","3"))
    assert("hey".equalsIgnoreCase(ans.get("fieldTwo").get.toString))
  }

  test( "FINAL - simple json object - string and boolean"){
    val jsonString = "{\n  \"fieldOne\" : true,\n  \"fieldTwo\" : \"true\"\n}"
    val firstStep = JsonLexer.lex(jsonString, List[Any]())
    val ans = JsonParser.parse(firstStep)
    assert(ans.get("fieldOne").get == true)
    assert("true".equalsIgnoreCase(ans.get("fieldTwo").get.toString))
  }

}
