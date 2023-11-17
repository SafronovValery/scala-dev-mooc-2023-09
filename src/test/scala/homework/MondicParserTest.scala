package homework

import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import scala3_1.TestExecution._



val str =
  """1997;Ford;E350;ac, abs, moon;3000
    |1996; Jeep; Grand Cherokee; MUST SELL! air, moon roof, loaded; 4799
    |2013; Opel; Meriva; turbo ; 700
    |1970; Москвичь ; 412 ; старье ; 10
    |1975; ВАЗ ; 2106; классика ; 20""".stripMargin

class MonadicParserTest extends AnyFlatSpec {
  "MonadParser" should "должен соответствовать ожидаемому поведению" in {

   assert(split(str).map(parser.parse) === Array(
     Car(1997,"Ford","E350","ac, abs, moon",3000.0f),
     Car(1996," Jeep"," Grand Cherokee"," MUST SELL! air, moon roof, loaded", 4799.0f ),
     Car(2013," Opel", " Meriva", " turbo ", 700.0f),
     Car(1970, " Москвичь ", " 412 ", " старье ", 10.0f),
     Car(1975, " ВАЗ ", " 2106", " классика ", 20.0f)
   ))

    assert(str.splitText.map(parser.parse) === Array(
      Car(1997, "Ford", "E350", "ac, abs, moon", 3000.0f),
      Car(1996, " Jeep", " Grand Cherokee", " MUST SELL! air, moon roof, loaded", 4799.0f),
      Car(2013, " Opel", " Meriva", " turbo ", 700.0f),
      Car(1970, " Москвичь ", " 412 ", " старье ", 10.0f),
      Car(1975, " ВАЗ ", " 2106", " классика ", 20.0f)
    ))


  }
}


