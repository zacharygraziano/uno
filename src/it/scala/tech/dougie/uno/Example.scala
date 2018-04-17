package tech.dougie.uno

import java.io.PrintWriter

object Example extends App {
  val pw = new PrintWriter(new java.io.File(s"${System.getenv("HOME")}/uno.txt"))
  val uno = Uno(numPlayers = 4)
  uno.run.zipWithIndex.foreach {
    case (gs, roundNum) =>
      pw.println(s"Round number ${roundNum + 1}")
      gs.history.sliding(2).map(summarize).foreach(pw.println)
      val winner = gs.players.find(_.hand.seq.isEmpty).get
      pw.println(s"$winner won round ${roundNum + 1} with score ${winner.score} \n")
  }
  val winner = uno.run.last.players.maxBy(_.score)
  pw.println(s"$winner won with score ${winner.score}")
  pw.close()

  def summarize(change: Seq[RoundState]): String = {
    val Seq(before, after) = change
    val whoMoved = before.player
    val afterMove = after.players.find(_.id == whoMoved.id).get
    val whatTheyPlayed = after.faceCard
    val howManyCards = after.others.find(_.id == whoMoved.id).map(_.hand.seq.length).get
    s"$whoMoved taking a turn:\n\t${whoMoved.hand}\nPlaying a ${whatTheyPlayed}\n\t${afterMove.hand}\n\n"
  }
}
