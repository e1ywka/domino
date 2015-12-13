package ru.agafontsev

import scala.util.Random

package object domino {
  val possibleDominos = {
    var dominos = Set.empty[Domino]
    for {
      left <- 0 to 6
      right <- left to 6
    } {
      dominos = dominos + Domino(left, right)
    }
    dominos
  }

  def randomSuit(count: Int) = {
    require(count > 0 && count <= 28)
    def nextDomino(availableDominos: Set[Domino], r: Random): Domino = {
      val ds =  availableDominos.toList
      ds(r.nextInt(ds.length))
    }

    if (count == 28) {
      possibleDominos
    } else {
      val random = new Random()
      var suit = Set.empty[Domino]
      (0 until count) foreach(_ => suit = suit + nextDomino(possibleDominos -- suit, random))
      suit
    }
  }
}