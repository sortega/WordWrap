package wordwrap

object Wrapper {

  def wrap(text: String, columnWidth: Int): String = {
    require(columnWidth > 0, "Column with should be non-negative")

    case class Line(value: String) {
      def canHold(token: String) = value.length + 1 + token.length <= columnWidth
      def append(token: String) = Line(value = value + " " + token)
    }

    case class Text(lines: Vector[Line] = Vector.empty) {
      def append(token: String): Text =
        if (lines.lastOption.exists(_.canHold(token)))
          Text(lines.init :+ lines.last.append(token))
        else Text(lines = lines :+ Line(token))
      override def toString = lines.map(_.value).mkString("\n")
    }

    def tokenize(text: String): Seq[String] =
      text.split("\\s+").flatMap(_.grouped(columnWidth))

    def layOut(tokens: Seq[String]): Text =
      tokens.foldLeft(Text())(_ append _)

    layOut(tokenize(text)).toString
  }
}
