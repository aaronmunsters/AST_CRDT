package AST

trait TX[V] {
  def publish(value: Seq[V]): Unit
}
