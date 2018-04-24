package htmltable._3

sealed trait Token

final case class Opening(name: String) extends Token {
  override def toString: String = s"<$name>"
}

final case class Closing(name: String) extends Token {
  override def toString: String = s"</$name>"
}

final case class Text(text: String) extends Token {
  override def toString: String = text
}

object Tokenizer {

  def apply(string: String): Tokenizer = new Tokenizer(string)
}

class Tokenizer(string: String) {

  implicit private class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def tokenize: Vector[Token] = tokenize(string.trim, Vector.empty[Token])

  private[this] def tokenize(string: String, acc: Vector[Token]): Vector[Token] = {
    string match {
      case r"^$$" => // empty string
        acc
      case r"^<\s*([A-Za-z0-9]+)\s*>$name(.*)$rest" => // opening tag
        tokenize(rest, acc :+ Opening(name))
      case r"^</\s*([A-Za-z0-9]+)\s*>$name(.*)$rest" => // closing tag
        tokenize(rest, acc :+ Closing(name))
      case r"^([^<]+)$text(.*)$rest" => // some other text without '<'
        tokenize(rest, acc :+ Text(text.trim))
      case unexpected =>
        throw new IllegalStateException(s"HTML not well-formed: unexpected input: >>>>>$unexpected")
    }
  }
}
