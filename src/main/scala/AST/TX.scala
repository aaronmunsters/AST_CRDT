package AST

trait TX[V] {
  def publish(value: Seq[V]): Unit
  def subscribe(callback: Seq[V] => Unit): Unit
}
