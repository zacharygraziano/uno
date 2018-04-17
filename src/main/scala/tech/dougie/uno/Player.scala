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

/** The strategy a player uses.
  * @param playStrategy A function that is used to determine which cards players ought to
  *                 play. The function takes as arguments all the cards in the player's hand
  *                 that they may play this turn as well as the state of the game. It must either
  *                 return a card that is in the hand and compatible with the face card or None if
  *                 the player does not wish to play a card.
  * @param wildStrategy A function that determines how the players pick the active color
  *                  when a wild card is played.
  */
final case class PlayerConfig(
  playStrategy: (Seq[Card], RoundState) => Option[Card],
  wildStrategy: RoundState => Color
)

final case class Hand(cards: Map[Card, Int]) {
  def count(card: Card): Int = cards.getOrElse(card, 0)
  lazy val seq
    : Vector[Card] = cards.flatMap { case (card, count) => Vector.fill(count)(card) }.toVector
  def -(card: Card): Hand =
    Hand(
      cards
        .get(card)
        .map(
          c => if (c != 1) cards.updated(card, c - 1) else cards - card
        )
        .getOrElse(cards))
  def +(card: Card): Hand =
    Hand(cards.get(card).map(c => cards.updated(card, c + 1)).getOrElse(cards + (card -> 1)))
  def ++(cards: Seq[Card]): Hand = cards.foldLeft(this)((acc, card) => acc + card)
  override def toString: String = seq.mkString("[", ", ", "]")
}
object Hand {
  def apply(cards: Card*): Hand = Hand(
    cards.groupBy(identity).mapValues(_.length)
  )
}

final case class Player private[uno] (
  id: Int,
  score: Int = 0,
  hand: Hand = Hand(Map.empty[Card, Int]),
  playStrategy: (Seq[Card], RoundState) => Option[Card],
  wildStrategy: RoundState => Color) {
  def addPoints(more: Int): Player = copy(score = score + more)
  override def toString: String = s"player $id"
}
