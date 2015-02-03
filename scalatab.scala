package scalatab

case class Tab(
    headings: Vector[String], 
    table: Vector[Vector[String]], 
    separator: String = ";")
extends Serializable {
  def apply(row: Int) = table(row)
  def apply(row: Int, col: Int) = table(row)(col)
  def mapRow[T](f: Vector[String] => T): Vector[T] = table.map(f)
  def map[T](f: String => T): Vector[Vector[T]] = table.map(_.map(f))
  def toString(separator: String) = 
    headings.mkString("",separator,"\n") +
    table.map(_.mkString(separator)).mkString("\n")
  override def toString = toString(separator)
}

object Tab {
  def loadLines(fileName:String): Vector[String] = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.getLines.toVector
    source.close
    lines
  }
  def load(fileName: String, separator: String = ";", hasHeadingRow: Boolean = true): Tab = {
    val lines: Vector[String] = loadLines(fileName) 
    val headings: Vector[String] = 
      if (hasHeadingRow) lines.headOption.getOrElse("").split(separator).toVector
      else Vector()
    val h = if (hasHeadingRow) 1 else 0
    val table: Vector[Vector[String]] =
      lines.drop(h).map(_.split(separator).toVector).toVector
    Tab(headings, table, separator)
  }
}
