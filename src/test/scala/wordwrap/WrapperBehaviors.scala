package wordwrap

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, ShouldMatchers}

trait WrapperBehaviors extends PropertyChecks with ShouldMatchers { this: FlatSpec =>

  def aWordWrapper(wrapper: (String, Int) => String): Unit = {
    val texts = Gen.listOf(Gen.alphaStr.suchThat(_.nonEmpty))
    val lines = texts.map(_.mkString(" "))
    val columnWidths = Gen.posNum[Int]

    it should "throw for zero or negative column widths" in new {
      val nonPositiveWidths = Gen.choose(Integer.MIN_VALUE, 0)
      forAll(lines, nonPositiveWidths) { (text: String, width: Int) =>
        an [IllegalArgumentException] shouldBe thrownBy {
          Wrapper.wrap(text, width)
        }
      }
    }

    it should "should output the same input text except for whitespace" in new {
      forAll(lines, columnWidths) { (text: String, width: Int) =>
        removeWhitespace(Wrapper.wrap(text, width)) shouldBe removeWhitespace(text)
      }
    }

    it should "output lines up to the limit" in new {
      forAll(lines, columnWidths) { (text: String, width: Int) =>
        val lines = Wrapper.wrap(text, width).lines.toList
        withClue(s"$lines exceeds $width columns:") {
          lines.forall(_.length <= width) shouldBe true
        }
      }
    }

    it should "break words only if they don't fit in a line" in new {
      forAll(texts, columnWidths) { (text: List[String], width: Int) =>
        val textWithLongWordsSplit = text.flatMap(word => word.grouped(width))
        wordsOf(Wrapper.wrap(text.mkString(" "), width)) shouldBe textWithLongWordsSplit
      }
    }

    it should "be idempotent" in new {
      forAll(lines, columnWidths) { (text: String, width: Int) =>
        val wrappedText = Wrapper.wrap(text, width)
        Wrapper.wrap(wrappedText, width) shouldBe wrappedText
      }
    }
  }

  private def removeWhitespace(text: String) = text.replaceAll("\\s", "")

  private def wordsOf(text: String) = text.split("\\s").filterNot(_.isEmpty)
}
