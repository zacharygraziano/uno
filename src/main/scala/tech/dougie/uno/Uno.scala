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

import scala.util.Random

final class Uno private (
  allPlayers: Vector[Player],
  gameOver: GameState => Boolean = Uno.DefaultEnd,
  cardsInHand: Int) {
  import Uno.when

  /** Run the entire game. */
  lazy val run: Stream[GameState] =
    Stream.iterate(initialGameState)(playRound).tail.takeWhile(!gameOver(_))

  private val numPlayers = allPlayers.length
  require(
    numPlayers >= 2 && numPlayers <= 10,
    "Cannot play with fewer than 2 or greater than 10 players")

  /** Play a single round. A round itself is a Stream[RoundState].
    * @param gameState the state of the game at the end of the previous round
    */
  def playRound(gameState: GameState): GameState = {
    val roundStream = Stream.iterate(startRound(gameState))(takeMove)
    val (beforeEnd, rest) = roundStream.splitAt(roundStream.indexWhere(roundOver))
    val round = beforeEnd #::: rest.take(1)
    val finalPlayers = round.last.players
    val score = finalPlayers.foldLeft(0)((total, player) =>
      total + player.hand.seq.foldLeft(0)((acc, c) => acc + c.value))
    val winner = finalPlayers.indexWhere(_.hand.seq.isEmpty)
    GameState(finalPlayers.updated(winner, finalPlayers(winner).addPoints(score)), round)
  }

  def roundOver(roundState: RoundState): Boolean = {
    (roundState.player +: roundState.others).exists(_.hand.cards.isEmpty)
  }

  lazy val initialGameState = GameState(allPlayers, Stream.empty)

  def startRound(gameState: GameState): RoundState = {
    val (deck, players) =
      deal(allCardsSeq, gameState.players.map(_.copy(hand = Hand())).sortBy(_.id))

    // In the event the top-card is not numbered, we throw away the previous start-state result
    // and start all over again. This is probably not correct.
    deck.head.cardColor
      .map { color =>
        if (deck.head.isWild || deck.head.isAction) startRound(gameState)
        else RoundState(deck.tail, players.head, deck.head, players.tail, color)
      }
      .getOrElse(startRound(gameState))
  }

  def takeMove(roundState: RoundState): RoundState = {
    val (afterDraw, playable) =
      playableCards(roundState.player, roundState.faceCard, roundState.activeColor) match {
        case Seq() =>
          val (s, nextCard) = drawCards(roundState, draw = 1, newMove = false)
          s ->
            when(compatible(nextCard.head, s.faceCard, s.activeColor))(nextCard.head).toSeq
        case p => roundState -> p
      }
    val chosenCard = afterDraw.player.playStrategy(playable, afterDraw)
    chosenCard
      .fold(afterDraw.next) { card =>
        require(
          afterDraw.player.hand.cards.contains(card),
          "Cannot play a card that is not in the current player's hand.")
        require(
          compatible(card, roundState.faceCard, roundState.activeColor),
          "Cannot play a card that is not compatible with the face card."
        )
        nextPlayer(card, playCard(card, afterDraw))
      }
  }

  def playCard(card: Card, roundState: RoundState): RoundState = card match {
    case Wild | WildDraw4 =>
      (roundState + card).copy(
        player = roundState.player.copy(hand = roundState.player.hand - card),
        activeColor = roundState.player.wildStrategy(roundState))
    case _ =>
      (roundState + card).copy(
        player = roundState.player.copy(hand = roundState.player.hand - card)
      )
  }

  def playableCards(player: Player, faceCard: Card, activeColor: Color): Seq[Card] = {
    player.hand.seq.filter(c => compatible(c, faceCard, activeColor))
  }

  // Return the new game state if a turn terminating action occurred.
  def nextPlayer(cardPlayed: Card, roundState: RoundState): RoundState = {
    import roundState._
    cardPlayed match {
      case ActionCard(Draw2, _) => drawTurn(roundState, draw = 2)
      case WildDraw4            => drawTurn(roundState, draw = 4)
      case ActionCard(Skip, _)  => drawTurn(roundState, draw = 0)
      case ActionCard(Reverse, _) if numPlayers != 2 =>
        val (nextPlayer, rest) = others.reverse.splitAt(1)
        roundState.copy(player = nextPlayer.head, others = rest :+ player)
      case _ =>
        roundState.next
    }
  }

  def compatible(card: Card, card1: Card, activeColor: Color): Boolean = {
    (card, card1) match {
      case (c1, _) if c1.isWild      => true
      case (c1, face) if face.isWild => activeColor == c1.cardColor.get // Non wild has a color.
      case (c1, c2) if c1.cardColor.exists(col => c2.cardColor.forall(col2 => col == col2)) =>
        true
      case (ActionCard(act1, _), ActionCard(act2, _)) if act1 == act2 => true
      case (Numbered(n1, _), Numbered(n2, _))                         => n1 == n2
      case _                                                          => false
    }
  }

  def drawCards(
    roundState: RoundState,
    draw: Int,
    newMove: Boolean = true): (RoundState, Seq[Card]) = {
    import roundState._
    val (drawn, rest) = drawFromDeck(1, roundState)
    val (nextPlayer, nextOthers) =
      if (newMove) others.head -> (others.tail :+ player.copy(hand = player.hand ++ drawn))
      else player.copy(hand = player.hand ++ drawn) -> others
    RoundState(rest, nextPlayer, roundState.faceCard, nextOthers, activeColor) -> drawn
  }

  def drawTurn(
    roundState: RoundState,
    draw: Int
  ): RoundState = {
    import roundState._
    val (drawn, rest) = drawFromDeck(draw, roundState)
    if (numPlayers != 2) {
      val victim = others.head
      roundState.copy(
        player = others.tail.head,
        others = others.tail.tail :+ player :+ victim.copy(hand = victim.hand ++ drawn),
        pile = rest
      )
    } else {
      val victim = others.head
      roundState.copy(
        player = player,
        others = Vector(victim.copy(hand = victim.hand ++ drawn)),
        pile = rest
      )
    }
  }

  lazy val allCards: Map[Card, Int] = {
    Color.all.flatMap { c =>
      val numbered = (1 to 9).map(n => Numbered(n, c) -> 2)
      val action = Seq(Skip, Reverse, Draw2).map(action => ActionCard(action, c) -> 2)
      (action ++ numbered :+ (Wild -> 4)) :+ (WildDraw4 -> 4) :+ (Numbered(0, c) -> 1)
    }.toMap
  }

  lazy val allCardsSeq: Vector[Card] = Hand(allCards).seq

  // Get a deck and some players with cards.
  def deal(deck: Vector[Card], players: Vector[Player]): (Vector[Card], Vector[Player]) = {
    val shuffled = Random.shuffle(deck)
    players.foldLeft((shuffled, Vector.empty[Player])) {
      case ((currentDeck, accplayers), player) =>
        val (hand, remaining) = currentDeck.splitAt(cardsInHand)
        (remaining, accplayers :+ player.copy(hand = Hand(hand: _*)))
    }
  }

  // Might need to reshuffle the discard pile if there aren't enough cards left.
  def drawFromDeck(draw: Int, roundState: RoundState): (Seq[Card], Seq[Card]) = {
    import roundState._
    if (draw < pile.length) roundState.pile.splitAt(draw)
    else pile -> newDrawPile(pile ++ players.flatMap(_.hand.seq) :+ faceCard)
  }

  def newDrawPile(cardsInPlay: Seq[Card]): Seq[Card] = Random.shuffle {
    cardsInPlay
      .foldLeft(allCards) { (acc, card) =>
        acc.updated(card, allCards(card) - 1)
      }
      .flatMap { case (card, count) => Seq.fill(count)(card) }
      .toSeq
  }
}
object Uno {

  /** A game of UNO.
    * @param numPlayers The number of players in the game. Must be between 2 and 10.
    * @param playerStrategy The strategy that all players will use.
    * @param gameOver A function that determines whether the game has ended.
    * @param cardsInHand How many cards the players start with.
    */
  def apply(
    numPlayers: Int,
    playerStrategy: PlayerConfig = DefaultConfig,
    gameOver: GameState => Boolean = DefaultEnd,
    cardsInHand: Int = 7): Uno = {
    val players = Vector.tabulate(numPlayers)(
      i =>
        Player(
          id = i,
          playStrategy = playerStrategy.playStrategy,
          wildStrategy = playerStrategy.wildStrategy))
    new Uno(players, gameOver, cardsInHand)
  }

  /**
    * A game of UNO.
    * @param playerStrategies A list of strategies, one for each player.
    * @param gameOver Criteria for game to end.
    * @param cardsInHand Number of cards each player gets at start of round.
    */
  def apply(
    playerStrategies: Seq[PlayerConfig],
    gameOver: GameState => Boolean,
    cardsInHand: Int): Uno = {
    val players = playerStrategies.zipWithIndex.map {
      case (PlayerConfig(play, wild), id) =>
        Player(id = id, playStrategy = play, wildStrategy = wild)
    }.toVector
    new Uno(players, gameOver, cardsInHand)
  }

  lazy val DefaultConfig: PlayerConfig = PlayerConfig(DefaultStrategy, RandomWild)

  /** Pick a random color when a wild is played. */
  def RandomWild: RoundState => Color = _ => Color.all(Random.nextInt(Color.all.length))

  /** Shed highest valued cards first to minimize risk. */
  def DefaultStrategy(playable: Seq[Card], state: RoundState): Option[Card] = {
    when(playable.nonEmpty)(playable.maxBy(_.value))
  }

  /** The official UNO end rule: one player has at least 500 points. */
  def DefaultEnd(state: GameState): Boolean = state.players.exists(_.score >= 500)

  private[uno] def when[A](expr: Boolean)(body: => A): Option[A] =
    if (expr) Some(body) else None
}
