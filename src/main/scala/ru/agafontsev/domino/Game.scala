package ru.agafontsev.domino

import ru.agafontsev.domino.Game.DominoTable

/**
 * Игра - определение, что базар раскладывается по правилам домино.
 * Внутреннее представление базара - неориентированный граф, где каждая кость представляет одно ребро.
 * Используем теорему Эйлера для определения возможности обхода всех вершин графа.
 * Внутренне представление базара реализовано в виде двумерного массива. Индексы массива - точки на каждой части кости.
 * Наличие определенной кости определяется по величине значения на пересечении индксов двумерного массива.
 * Для дублей указывается "2", для остальных костей "1".
 */
object Game {
  type DominoTable = Array[Array[Int]]

  /**
   * Конвертация базара во внутреннее представление в виде графа
   * @param dominos
   * @return
   */
  private[domino] def fillMatrix(dominos: List[Domino]) = {
    val dominosMatrix: DominoTable = Array.fill(7, 7)(0)
    dominos foreach { d =>
      val used = if (d.isDouble) 2 else 1
      dominosMatrix(d.left)(d.right) = used
      dominosMatrix(d.right)(d.left) = used
    }
    dominosMatrix
  }

  /**
   * Определение, что граф связанный.
   * Для определения связанности графа используется обход графа в глубину.
   * @param table
   * @return
   */
  private[domino] def isConnected(table: DominoTable): Boolean = {
    var visited: List[Int] = Nil
    val available: Int = table.map(v => if (v.contains(1)) 1 else 0).sum
    def deepSearch(vertex: Int): Unit = {
      if (!visited.contains(vertex)) {
        val connects = table(vertex).zipWithIndex.filter(_._1 > 0)
        if (connects.length > 0) {
          visited = vertex +: visited
          connects.foreach((e: (Int, Int)) => deepSearch(e._2))
        }
      }
    }

    0 to 6 find { i => table(i).exists(_ > 0) } match {
      case Some(v) =>
        deepSearch(v)
        available == visited.size
      case None => false
    }

  }

  private[domino] def copy(dt: DominoTable): DominoTable = {
    val newCopy = Array.fill(7, 7)(0)
    for (i <- 0 to 6) {
      dt(i).copyToArray(newCopy(i))
    }
    newCopy
  }

  /**
   * Игра - определение, что базар раскладывается по правилам домино.
   * Внутреннее представление базара - неориентированный граф, где каждая кость представляет одно ребро.
   * Используем теорему Эйлера для определения возможности обхода всех вершин графа.
   * Внутренне представление базара реализовано в виде двумерного массива. Индексы массива - точки на каждой части кости.
   * Наличие определенной кости определяется по величине значения на пересечении индксов двумерного массива.
   * Для дублей указывается "2", для остальных костей "1".
   * @param dominos набор домино, максимум 28 костей.
   * @return новая "игра".
   */
  def apply(dominos: List[Domino]): Game = {
    require(dominos.size >= 2 && dominos.size <= 28)
    new Game(fillMatrix(dominos))
  }
}

class Game private (private val dominosMatrix: DominoTable) {
  import Game._

  /**
   * Определение, что граф, полученный из базара, явдяется связанным.
   * Это обязательное условие для Эйлерова цикла и пути.
   */
  private val connected = isConnected(dominosMatrix)
  /**
   * Количество вершин с четной степенью.
   */
  private val oddVertexes = dominosMatrix.map(_.sum).count(_ % 2 != 0)
  /**
   * Возвращает true, если все домино из "базара" можно выложить в одну линию.
   * Нахождение эйлерова пути в графе.
   */
  val q2 = connected && oddVertexes <= 2
  /**
   * Возвращает true, если все домино из базара можно выложить в замкнутую линию.
   * Нахождение эйлерова цикла в графе.
   * Если q3=true, то и q2=true,
   */
  val q3 = connected && oddVertexes == 0
  /**
   * Возвращает true, если все домино из базара соединяются по правилам домино.
   * Граф либо должен иметь эйлеров путь, либо мы начинаем убирать по одной кости и проверять,
   * не стал ли полученный базар выстраиваться в граф с эйлеровым путем.
   */
  val q1 = connected && (q2 || solveQ1())

  private def solveQ1(): Boolean = {
    dominosMatrix.map(_.sum).zipWithIndex.filter(_._1 % 2 != 0).exists { v =>
      dominosMatrix(v._2).zipWithIndex.filter(_._1 == 1).exists { vv =>
        val m = copy(dominosMatrix)
        m(v._2)(vv._2) = 0
        m(vv._2)(v._2) = 0
        new Game(m).q1
      }
    }
  }

  override def toString = {
    val res: StringBuilder = new StringBuilder
    for (i <- 0 to 6; j <- 0 to 6) {
      res.append(dominosMatrix(i)(j).toString)
      if (j == 6) {
        res.append("|")
      }

    }
    res.toString()
  }
}
