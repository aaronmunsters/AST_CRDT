import AST.Parse.Parser

object Formatter {
  def format(source: String, indentation: Int = 4): Option[String] = {
    var uniqueIdentity = 0
    val getUniqueIdentity = () => { uniqueIdentity += 1; uniqueIdentity }
    Parser.parseSchemeSmall(source, getUniqueIdentity).map(_.toPrettyAstString(indentation))
  }
}
