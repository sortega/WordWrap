package wordwrap

import org.scalatest.FlatSpec

class WordWrapTest extends FlatSpec with WrapperBehaviors {
  "WordWrap" should behave like aWordWrapper(WordWrap.wrap)
}
