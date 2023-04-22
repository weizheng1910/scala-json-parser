import org.scalatest.funsuite.AnyFunSuite

class TestLex extends AnyFunSuite {

  // Unitary tests
  test("Test the lexString function") {
    val string = "\"hello\"1234"
    val ans = JsonLexer.lexString(List[Any](), string)
    assert(ans._1 == List("hello"))
    assert(ans._2 == "1234")
  }

  test("Test the lexInteger function"){
    val string = "1234 \n \"party:\"yes\""
    val ans = JsonLexer.lexInteger(List[Any](), string)
    assert(ans._1 == List("1234"))
  }

  test("Test the lex function on a JSON with integer values"){
    val jsonString = "{\n  \"fieldOne\" : 1,\n  \"fieldTwo\" : 2\n}"
    val firstStep = JsonLexer.lex(List[Any](), jsonString)
    assert(firstStep.length == 9)
  }

  test("Test the parseArray function using an array that contains an object"){
    val list = List("{","color",":","red","}","]")
    val ans = JsonParser.parseArray(list)
    assert(ans._1.length == 1)
    assert(ans._2.isEmpty)
  }

  test("Test the parseArray function using an array that contains simple strings"){
    val list = List("x",",","d",",","c",",","b","]")
    val ans = JsonParser.parseArray(list)
    assert(ans._1.length == 4)
    assert(ans._2.isEmpty)
  }

  test("Test the parseObject function"){
    val array = List("key",":","value","}")
    val result = JsonParser.parseObject(array)
    assert(result != null)
  }

  // Full Tests
  test("Parse a JSON that contains integer values"){
    val jsonString = "{\n  \"fieldOne\" : 1,\n  \"fieldTwo\" : 2\n}"
    val ans = JsonParser.parser(jsonString).asInstanceOf[Map[Any,Any]]
    assert(ans.get("fieldOne").contains("1"))
  }

  test("Parse a JSON that contains an array and a string value"){
    val jsonString = "{\n  \"fieldOne\" : [1,2,3]\n  \"fieldTwo\" : \"hey\"\n}"
    val firstStep = JsonLexer.lex(List[Any](), jsonString)
    val ans = JsonParser.parser(jsonString).asInstanceOf[Map[Any,Any]]
    assert(ans("fieldOne") == List("1","2","3"))
    assert("hey".equalsIgnoreCase(ans("fieldTwo").toString))
  }

  test( "Parse a simple object with boolean values"){
    val jsonString = "{\n  \"fieldOne\" : true,\n  \"fieldTwo\" : \"true\"\n}"
    val ans =  JsonParser.parser(jsonString).asInstanceOf[Map[Any,Any]]
    assert(ans("fieldOne") == true)
    assert("true".equalsIgnoreCase(ans("fieldTwo").toString))
  }

  test("Parse an array of objects"){
    val jsonString = "[\n\t{\n\t\t\"color\": \"red\",\n\t\t\"value\": \"test\"\n\t},\n\t{\n\t\t\"color\": \"green\",\n\t\t\"value\": \"test\"\n\t},\n\t{\n\t\t\"color\": \"blue\",\n\t\t\"value\": \"test\"\n\t},\n\t{\n\t\t\"color\": \"cyan\",\n\t\t\"value\": \"test\"\n\t},\n\t{\n\t\t\"color\": \"magenta\",\n\t\t\"value\": \"test\"\n\t},\n\t{\n\t\t\"color\": \"yellow\",\n\t\t\"value\": \"test\"\n\t},\n\t{\n\t\t\"color\": \"black\",\n\t\t\"value\": \"test\"\n\t}\n]"
    val ans = JsonParser.parser(jsonString).asInstanceOf[List[Any]]
    assert(ans.length == 7)
  }

}
