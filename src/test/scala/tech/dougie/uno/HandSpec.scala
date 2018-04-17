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

class HandSpec extends FunSpec with Matchers {
  describe("Hand") {
    it("should remove cards") {
      (Hand(Numbered(3, Blue), Numbered(5, Red), Numbered(2, Green)) - Numbered(2, Green)).seq should contain theSameElementsAs
        Vector(Numbered(3, Blue), Numbered(5, Red))
    }
    it("should remove cards when there's multiple of one card") {
      Hand(Wild, Wild, Wild) - Wild shouldBe Hand(Map((Wild: Card) -> 2))
    }
    it("should count how many cards there are") {
      Hand(Wild, Wild).count(Wild) shouldBe 2
      Hand(Numbered(5, Blue)).count(Wild) shouldBe 0
    }
    it("should add cards") {
      Hand(Wild, WildDraw4) + ActionCard(Reverse, Blue) shouldBe Hand(
        Map[Card, Int](Wild -> 1, WildDraw4 -> 1, ActionCard(Reverse, Blue) -> 1))
      Hand(Wild) + Wild shouldBe Hand(Map[Card, Int](Wild -> 2))
    }
    it("should make a hand out of a sequence") {
      Hand(Wild, Numbered(0, Blue), Numbered(1, Red), Numbered(1, Red)) shouldBe Hand(
        Map[Card, Int](Numbered(0, Blue) -> 1, Numbered(1, Red) -> 2, Wild -> 1))
    }
  }
}
