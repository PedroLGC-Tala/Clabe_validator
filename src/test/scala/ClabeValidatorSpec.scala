import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClabeValidatorSpec extends AnyFlatSpec with Matchers {

  "ClabeValidator" should "validate a correct CLABE" in {
    ClabeValidator.validateClabe("638180010142384005") shouldBe true
  }

  it should "reject an incorrect CLABE" in {
    ClabeValidator.validateClabe("123456789123456789") shouldBe false
  }

  it should "return the correct bank name for a given CLABE" in {
    ClabeValidator.getBankName("638180010142384005") shouldBe "NU MEXICO"
  }

  it should "throw an exception for an invalid bank code" in {
    an[Exception] should be thrownBy ClabeValidator.getBankName("12345678989")
  }

  it should "compute the correct control digit for a given CLABE" in {
    ClabeValidator.computeControlDigit("638180010142384005") shouldBe "5"
  }

  it should "detect if a string contains only digits" in {
    ClabeValidator.isANumber("638180010142384005") shouldBe true
    ClabeValidator.isANumber("notanumber") shouldBe false
  }
}
