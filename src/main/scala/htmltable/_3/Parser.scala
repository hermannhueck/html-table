package htmltable._3

object Parser {

  def indent(level: Int)(implicit pretty: Boolean): String = {
      if (!pretty)
        ""
      else if (level == 0)
        "\n"
      else
        "\n" + (1 to level).foldRight("")((i, acc) => "    " + acc)
    }

  type Tags = List[Tag]

  sealed trait Content {
    def prettyString(level: Int)(implicit pretty: Boolean): String
  }

  final case class StringContent(content: String) extends Content {
    override def prettyString(level: Int = 0)(implicit pretty: Boolean = false): String = content
  }

  final case class TagsContent(content: Tags) extends Content {
    override def prettyString(level: Int = 0)(implicit pretty: Boolean = false): String = {
      val result = content.map(_.prettyString(level)).mkString
      if (level == 0) result.trim else result
    }
  }

  final case class Tag(name: String, content: Content) {
    def prettyString(level: Int)(implicit pretty: Boolean): String = {
      val ind = indent(level)
      if (name.isEmpty)
        s"$ind${content.prettyString(level + 1)}"
      else
        s"$ind<$name>${content.prettyString(level + 1)}$ind</$name>"
    }
  }
}

import Parser._

trait Parser {
  def parse: TagsContent
}

object TreeParser {
  def apply(string: String) = new TreeParser(string)
}

class TreeParser(val string: String) extends Parser {

  override def parse: TagsContent = TagsContent(parse(Tokenizer(string).tokenize))

  private def parse(tokens: Vector[Token]): Tags = tokens match {

    case Vector() =>
      List.empty[Tag]

    case Vector(Text(text)) =>
      List(Tag("", StringContent(text)))

    case Opening(tokenName) +: tail =>
      val (innerContent, rest) = splitAtClosingToken(tokenName, tail)
      val tag = Tag(tokenName, TagsContent(parse(innerContent)))
      tag +: parse(rest)

    case unexpected =>
      throw new IllegalStateException(s"HTML not well-formed: unexpected token: $unexpected")
  }

  private def splitAtClosingToken(name: String, tokens: Vector[Token]): (Vector[Token], Vector[Token]) = {
    val index = tokens.indexOf(Closing(name))
    if (index < 0)
      throw new IllegalStateException(s"HTML not well-formed: no closing Tag found with name: $name")
    val (innerContent, rest) = tokens.splitAt(index)
    (innerContent, rest.drop(1)) // drop the closing tag
  }
}
