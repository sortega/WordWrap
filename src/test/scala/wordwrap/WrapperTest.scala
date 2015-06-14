package wordwrap

import org.scalatest.FlatSpec

class WrapperTest extends FlatSpec with WrapperBehaviors {
  "Wrapper" should behave like aWordWrapper(Wrapper.wrap)
}
