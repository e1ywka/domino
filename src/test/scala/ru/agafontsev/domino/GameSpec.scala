package ru.agafontsev.domino

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class GameSpec extends FlatSpec with Matchers {

  "Game" should "be played with appropriate amoun of bones" in {
    intercept[IllegalArgumentException] {
      Game(List(Domino(1, 2)))
    }
  }

  it should "test some random suit" in {
    val suit = randomSuit(Random.nextInt(28) + 1).toList
    Game(suit)
  }

  "28 dominos" should "connects into cycle line" in {
    val suit = possibleDominos.toList
    val game = Game(suit)
    game.q1 should be(true)
    game.q2 should be(true)
    game.q3 should be(true)
  }

  it should "check that all can be connected" in {
    val suit = List(Domino(1, 2), Domino(2, 2), Domino(2, 3), Domino(2, 4))
    val game = Game(suit)
    game.q1 should be(true)
    game.q2 should be(false)
    game.q3 should be(false)
  }

  it should "check that all can be connected into line" in {
    val suit = List(Domino(1, 2), Domino(2, 2), Domino(2, 3), Domino(3, 4))
    val game = Game(suit)
    game.q1 should be(true)
    game.q2 should be(true)
    game.q3 should be(false)
  }

  it should "check that all can be connected into cycle line" in {
    val suit = List(Domino(1, 2), Domino(2, 2), Domino(2, 3), Domino(1, 3))
    val game = Game(suit)
    game.q1 should be(true)
    game.q2 should be(true)
    game.q3 should be(true)
  }
}
