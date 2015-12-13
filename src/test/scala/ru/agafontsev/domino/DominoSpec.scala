package ru.agafontsev.domino

import org.scalatest.{FlatSpec, Matchers}

class DominoSpec extends FlatSpec with Matchers {

  "Domino" should "be equal to itself" in {
    Domino(0, 0) should be(Domino(0, 0))
    Domino(0, 1) should be(Domino(0, 1))
  }

  "Rotated dominos" should "be equal" in {
    Domino(0, 1) should be(Domino(1, 0))
  }

  "Domino" should "validate its` fields" in {
    intercept[IllegalArgumentException] {
      Domino(7, 0)
    }
    intercept[IllegalArgumentException] {
      Domino(-1, 0)
    }
    intercept[IllegalArgumentException] {
      Domino(0, 7)
    }
    intercept[IllegalArgumentException] {
      Domino(0, -1)
    }
  }

  "possibleDominos" should "return all available dominos" in {
    val dominos = possibleDominos
    dominos.size should be(28)
  }

  "Random suit" should "be generated" in {
    val suit = randomSuit(10)
    println(suit)
  }
}
