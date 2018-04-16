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

sealed trait Card {
  def isWild: Boolean = false
  def isAction: Boolean = true
  def value: Int
  def cardColor: Option[Color] = None
}
case object Wild extends Card {
  def value: Int = 50
  override def toString: String = "wild card"
  override def isWild: Boolean = true
}
case object WildDraw4 extends Card {
  def value: Int = 50
  override def toString: String = "wild draw 4 card"
  override def isWild: Boolean = true
}
final case class Numbered(num: Int, color: Color) extends Card {
  override def isAction: Boolean = false
  def value: Int = num
  override def cardColor: Option[Color] = Some(color)
  override def toString: String = s"$color $num"
}

final case class ActionCard(action: Action, color: Color) extends Card {
  def value: Int = 20
  override def cardColor: Option[Color] = Some(color)
  override def toString: String = s"$color ${action.toString.toLowerCase}"
}

sealed trait Action
case object Skip extends Action
case object Reverse extends Action
case object Draw2 extends Action

sealed trait Color
case object Red extends Color
case object Blue extends Color
case object Green extends Color
case object Yellow extends Color
object Color {
  val all: Seq[Color] = Seq(Red, Blue, Green, Yellow)
}
