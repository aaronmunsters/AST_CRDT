package AST

object TestUtils {
  def getIdGenerator: () => Int = {
    var id = 0
    () => {
      id += 1
      id
    }
  }
}
