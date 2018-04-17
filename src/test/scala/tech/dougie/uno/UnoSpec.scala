/*
 * Copyright (c) 2018 Zachary Graziano
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package tech.dougie.uno

import org.scalatest.{FunSpec, Matchers}

class UnoSpec extends FunSpec with Matchers {

  describe("Game initiation") {
    val game = Uno(numPlayers = 4)
    it("should create the correct number of cards") {
      game.allCardsSeq should have length 108
    }
    it("should not introduce new cards in the dealing process") {
      val (deck, players) =
        game.deal(
          game.allCardsSeq,
          game.startRound(game.initialGameState).player +: game
            .startRound(game.initialGameState)
            .others)
      deck ++ players.flatMap(_.hand.seq) should contain theSameElementsAs game.allCardsSeq
    }
    it("fails when too many or too few players are supplied") {
      an[IllegalArgumentException] shouldBe thrownBy(Uno(numPlayers = 1))
      an[IllegalArgumentException] shouldBe thrownBy(Uno(numPlayers = 11))
    }
    it("construction succeeds when any legal number of players is supplied") {
      (2 to 10).foreach { n =>
        noException shouldBe thrownBy(Uno(numPlayers = n))
      }
    }
  }

  describe("compatibility") {
    val game = Uno(numPlayers = 2)
    val compatible: (Card, Card) => Boolean = game.compatible(_, _, Red)
    it("anything is compatible with Wild") {
      game.allCardsSeq.foreach(c => compatible(Wild, c) shouldBe true)
    }
    it("two cards with the same symbol are compatible") {
      compatible(ActionCard(Skip, Blue), ActionCard(Skip, Red)) shouldBe true
      compatible(Numbered(2, Green), Numbered(2, Yellow)) shouldBe true
    }
    it("unrelated cards are not compatible") {
      compatible(ActionCard(Reverse, Green), ActionCard(Skip, Red)) shouldBe false
      compatible(Numbered(0, Blue), Numbered(1, Green)) shouldBe false
    }
    it("two cards with the same color are compatible") {
      compatible(ActionCard(Skip, Blue), Numbered(8, Blue)) shouldBe true
      compatible(ActionCard(Reverse, Blue), ActionCard(Skip, Blue)) shouldBe true
      compatible(Numbered(3, Red), Numbered(2, Red)) shouldBe true
    }
  }

  describe("2 player games.") {
    val game = Uno(numPlayers = 2)
    it("skip should skip players in a 2 player game") {
      val next = game.nextPlayer(ActionCard(Skip, Red), game.startRound(game.initialGameState))
      next.player.id shouldBe 0
    }
    it("reverse should be a no-op in a two player game") {
      val next = game.nextPlayer(ActionCard(Reverse, Green), game.startRound(game.initialGameState))
      next.player.id shouldBe 1
    }
  }

  describe("3+ player games") {
    val game = Uno(numPlayers = 4)
    it("reverse should reverse order of player in a multiplayer game") {
      val next = game.nextPlayer(ActionCard(Reverse, Blue), game.startRound(game.initialGameState))
      next.player.id shouldBe 3
      next.others.map(_.id) shouldBe Vector(2, 1, 0)
      val anotherRound = game.nextPlayer(Numbered(4, Green), next)
      anotherRound.player.id shouldBe 2
      anotherRound.others.map(_.id) shouldBe Vector(1, 0, 3)
    }
    it("skip should skip players in a multi player game") {
      val next = game.nextPlayer(ActionCard(Skip, Red), game.startRound(game.initialGameState))
      next.player.id shouldBe 2
      next.others.map(_.id) shouldBe Vector(3, 0, 1)
    }
  }

  describe("state correctness") {
    val game = Uno(numPlayers = 5)
    val ranIt = game.playRound(game.initialGameState).history
    it("should never drop players") {
      ranIt.foreach { gs =>
        gs.player.id +: gs.others.map(_.id) should contain theSameElementsAs (0 until 5)
      }
    }
  }

  describe("Supplying custom strategies") {
    def illegalStrategy(playable: Seq[Card], state: RoundState): Option[Card] = {
      Some(Numbered(12, Red))
    }
    val badgame = Uno(numPlayers = 3, PlayerConfig(illegalStrategy, Uno.RandomWild))
    it("should not allow an illegal move to be made") {
      an[IllegalArgumentException] shouldBe thrownBy(
        badgame.playRound(badgame.initialGameState).history.toList)
    }
    val wildgame =
      Uno(numPlayers = 3, PlayerConfig(Uno.DefaultStrategy, wildStrategy = _ => Red))
    it("should listen to a custom wild strategy") {
      wildgame
        .playRound(wildgame.initialGameState)
        .history
        .dropWhile(_.activeColor != Red)
        .foreach { gs =>
          gs.activeColor shouldBe Red
        }
    }
    def keepHighValue(playable: Seq[Card], state: RoundState): Option[Card] = {
      if (playable.nonEmpty) Some(playable.minBy(_.value)) else None
    }
    val strategy1 = PlayerConfig(Uno.DefaultStrategy, wildStrategy = _ => Red)
    val strategy2 = PlayerConfig(keepHighValue, wildStrategy = _ => Blue)
    val game = Uno(Seq(strategy1, strategy2), Uno.DefaultEnd _, 7)
    val firstRound = game.run.head.history

    it("should listen to the play strategy for one player") {
      val init = firstRound.head
      val playable = game.playableCards(init.player, init.faceCard, init.activeColor)
      val cardPlayed = init.player.playStrategy(playable, init)
      cardPlayed.map(_.value) shouldBe Uno.when(playable.nonEmpty)(playable.map(_.value).max)
    }
    it("should listen to the play strategy for another player") {
      // The probability of this .get failing is so incredibly low
      // (when was the last time only one person ever took a turn in a two player game?)
      val otherPlayerState = firstRound.find(_.player.id == 1).get
      val playable = game.playableCards(
        otherPlayerState.player,
        otherPlayerState.faceCard,
        otherPlayerState.activeColor)
      val cardPlayed = otherPlayerState.player.playStrategy(playable, otherPlayerState)
      cardPlayed.map(_.value) shouldBe Uno.when(playable.nonEmpty)(playable.map(_.value).min)
    }
    it("should listen to wild strategies") {
      firstRound.sliding(2).foreach {
        case Seq(before, after) =>
          if (after.faceCard.isWild) {
            if (before.player.id == 0) after.activeColor shouldBe Red
            else after.activeColor shouldBe Blue
          }
      }
    }
  }
}
