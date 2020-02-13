package jbrief

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model.Element

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import cats.Applicative
import cats.data.Validated

import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import doobie._
import doobie.implicits._

import jbrief.db.Statements

object Main extends App {
    
    implicit val cs = IO.contextShift(ExecutionContexts.synchronous)

    // A transactor that gets connections from java.sql.DriverManager and executes blocking operations
    // on an our synchronous EC. See the chapter on connection handling for more info.
    val xa = Transactor.fromDriverManager[IO](
        "org.postgresql.Driver",     // driver classname
        "jdbc:postgresql:jbrief",     // connect URL (driver-specific)
        args(0),                  // user
        args(1),                          // password
    )

    val browser = JsoupBrowser()
    val doc = browser get (args(2))
    val game_id = args(2).split("=")(1).toInt

    val contestant_elements: List[Element] = (doc >> elementList("p.contestants"))

    val contestants = ContestantExtractor.extractContestants(contestant_elements)

    if(contestants.exists(_.isInvalid)) throw new RuntimeException("Cannot extract contestants.")

    val unpacked_contestants = contestants.map(_.toEither).map(_.right.get)

    val insert_contestants = Statements.insertContestant(xa)(_)
    val insert_turn = Statements.insertTurn(xa)(_)
    val insert_question = Statements.insertQuestion(xa)(_)

    unpacked_contestants.map(insert_contestants)

    val contestant_names: Map[String, Int] = contestants.map((c: ContestantExtractor.ExtractionResult[Contestant]) => c match {
        case Valid(c: Contestant) => (c.first_name, c.contestant_id)
        case _ => throw new RuntimeException("Invalid Contestants")
    }).toMap

    val clue_elements: List[Element] = (doc >> elementList("td.clue"))

    val extract_clue = ClueExtractor.extract_question(contestant_names, game_id, _ : Element)

    val clues = clue_elements.map(extract_clue)

    val invalid_extractions = clues.filter(_.isLeft)

    if(invalid_extractions.length == 0 ||
       invalid_extractions.map(_.left.get.string).forall(_ == "Cannot extract mouse over")) {

        val valid_extractions = clues.filter(_.isRight).map(_.right.get)

        val turns = valid_extractions.flatMap(_._1)
        val questions = valid_extractions.map(_._2)

        questions.map(insert_question)
        turns.map(insert_turn)
       }

    else {
        throw new RuntimeException(" One invalid turn or question was found")
    }
    
}