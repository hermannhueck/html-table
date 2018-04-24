package htmltable._2

object HtmlTable2 extends App {

  case class Tag[Valid <: Tag[_]](name: String) {
    def apply(elems: Node[Valid]*): Elem[this.type] = Elem[this.type](this, elems)
  }

  object Node {

    import scala.language.implicitConversions

    implicit def strToText(str: String): Text = Text(str)
  }

  sealed trait Node[-T <: Tag[_]]

  case class Elem[T <: Tag[_]](tag: T, children: Seq[Node[_]]) extends Node[T] {
    override def toString: String = s"<${tag.name}>${children.mkString}</${tag.name}>"
  }

  case class Text(text: String) extends Node[TextOnly] {
    override def toString: String = text
  }

  trait TextOnly extends Tag[Nothing] // phantom type

  val table = Tag[thead.type with tbody.type]("table")
  val tbody = Tag[tr.type]("tbody")
  val thead = Tag[tr.type]("thead")
  val tr = Tag[td.type with th.type]("tr")
  val th = Tag[TextOnly]("th")
  val td = Tag[TextOnly]("td")

  val html =
    table(
      thead(
        tr(th("Developers"))
      ),
      tbody(
        //td("Not allowed!"),
        tr(th("Name"), th("Age")),
        tr(td("Jon"), td("35")),
        tr(td("Hermann"), td("64"))
      )
    )

  println(html)
}
