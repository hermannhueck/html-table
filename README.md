# Building an HTML table leveraging the Scala type system

This small project is experimenting with the capabilities of the Scala
type system. It creates an html table using the type system to ensure
only allowed child tags for each given tag (htmltable.{_1, _2}).
In the 3rd package (htmltable._3) I implemented a small parser to parse
the tree of tags again in order to finally pretty-print the tags.
