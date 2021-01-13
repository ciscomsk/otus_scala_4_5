package homeworks.collections

import homeworks.collections.task_caesar.{decrypt, encrypt}
import org.scalatest.flatspec.AnyFlatSpec

class caesar_test extends AnyFlatSpec {

  "caesar encryptor" should "encrypt and decrypt simple words positive offset" in {
    assert(encrypt("SCALA", 2) === "UECNC")
    assert(decrypt("UECNC", 2) === "SCALA")
  }

  "caesar encryptor" should "encrypt and decrypt simple words negative offset" in {
    assert(encrypt("SCALA", -2) === "QAYJY")
    assert(decrypt("QAYJY", -2) === "SCALA")
  }

  it should "encrypt and decrypt with little positive offset" in {
    assert(decrypt(encrypt("SCALA", 2), 2) === "SCALA")
  }

  it should "encrypt and decrypt with little negative offset" in {
    assert(decrypt(encrypt("SCALA", -2), -2) === "SCALA")
  }

  it should "encrypt and decrypt with big positive offset" in {
    assert(decrypt(encrypt("SCALAPENIO", 242), 242) === "SCALAPENIO")
  }

  it should "encrypt and decrypt with big negative offset" in {
    assert(decrypt(encrypt("SCALAPENIO", -242), -242) === "SCALAPENIO")
  }

  it should "encrypt and decrypt empty string positive offset" in {
    assert(decrypt(encrypt("", 1), 1) === "")
  }

  it should "encrypt and decrypt empty string negative offset" in {
    assert(decrypt(encrypt("", -1), 1) === "")
  }

  it should "compose encryptions positive offsets" in {
    assert(encrypt(encrypt("ILOVESCALA", 8), 20) === encrypt("ILOVESCALA", 28))
  }

  it should "compose encryptions negative offsets" in {
    assert(encrypt(encrypt("ILOVESCALA", -8), -20) === encrypt("ILOVESCALA", -28))
  }

  it should "compose encryptions combined offsets_1" in {
    assert(encrypt(encrypt("ILOVESCALA", 8), -20) === encrypt("ILOVESCALA", -12))
  }

  it should "compose encryptions combined offsets_2" in {
    assert(encrypt(encrypt("ILOVESCALA", -8), 20) === encrypt("ILOVESCALA", 12))
  }

  it should "consider cycles positive offset" in {
    assert(encrypt("XYZ", 3) === "ABC")
  }

  it should "consider cycles negative offset" in {
    assert(encrypt("ABC", -3) === "XYZ")
  }
}
