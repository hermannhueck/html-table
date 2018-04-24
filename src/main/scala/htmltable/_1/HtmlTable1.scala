package htmltable._1

object HtmlTable1 extends App {

  class Tag[Valid <: Tag[_]](val name: String) {
    def apply(elems: Node[Valid]*): Elem[this.type] = new Elem[this.type](this, elems)
  }

  object Node {
    import scala.language.implicitConversions
    implicit def strToText(str: String): Text = new Text(str)
  }

  sealed trait Node[-T <: Tag[_]]

  class Elem[T <: Tag[_]](val tag: T, val children: Seq[Node[_]]) extends Node[T] {
    override def toString: String = s"<${tag.name}>${children.mkString}</${tag.name}>"
  }

  class Text(val text: String) extends Node[TextOnly] {
    override def toString: String = text
  }

  trait TextOnly extends Tag[Nothing] // phantom type

  val table = new Tag[thead.type with tbody.type]("table")
  val tr = new Tag[td.type with th.type]("tr")
  val tbody = new Tag[tr.type]("tbody")
  val thead = new Tag[tr.type]("thead")
  val td = new Tag[TextOnly]("td")
  val th = new Tag[TextOnly]("th")

  println(
    table(
      thead(tr(th("Developers"))),
      tbody(
        //td("Not allowed!"),
        tr(th("Name"), th("Age")),
        tr(td("Jon"), td("35")),
        tr(td("Hermann"), td("64"))
      )
    )
  )
}
