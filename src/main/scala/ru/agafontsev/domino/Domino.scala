package ru.agafontsev.domino

/**
 * Модель кости в игре домино.
 * @param left количество точек на левой части кости.
 * @param right количетсво точек на правой части кости.
 */
case class Domino(left: Int, right: Int) {
  require(left >= 0 && left <= 6, "left field must be between 0 and 6")
  require(right >= 0 && right <= 6, "right field must be between 0 and 6")

  /**
   * Возвращает true, если кость является дублем.
   */
  val isDouble = left == right

  override def equals(obj: scala.Any): Boolean = obj match {
    case Domino(`left`, `right`) | Domino(`right`, `left`) => true
    case _ => false
  }
}
