package jbrief

import net.ruippeixotog.scalascraper.browser.{JsoupBrowser, Browser}
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

    def find_todays_link(browser: Browser,
                         current_season_link: String, todays_date: String): String = {
        
        val links = (browser.get(current_season_link)) >> elementList("a")
        links.filter(_.text.contains(todays_date))(0).attrs("href")
    }

    val browser = JsoupBrowser()

    val todays_link = find_todays_link(browser, args(2), args(3))
    val doc = browser get (todays_link)
    val game_id = todays_link.split("=")(1).toInt

    val contestant_elements: List[Element] = (doc >> elementList("p.contestants"))

    val contestants = ContestantExtractor.extractContestants(contestant_elements)

    if(contestants.exists(_.isInvalid)) throw new RuntimeException("Cannot extract contestants.")

    val unpacked_contestants = contestants.map(_.toEither).map(_.right.get)

    val insert_contestants = Statements.insertContestant(xa)(_)
    val insert_turn = Statements.insertTurn(xa)(_)
    val insert_question = Statements.insertQuestion(xa)(_)
    val insert_date = Statements.insertDate(xa)(_)

    val contestant_names: Map[String, Int] = contestants.map((c: ContestantExtractor.ExtractionResult[Contestant]) => c match {
        case Valid(c: Contestant) => (c.first_name, c.contestant_id)
        case _ => throw new RuntimeException("Invalid Contestants")
    }).toMap

    val clue_elements: List[Element] = (doc >> elementList("td.clue"))

    val extract_clue = ClueExtractor.extract_regular_question(contestant_names, game_id, _ : Element)

    val clues = clue_elements.map(extract_clue)

    val extract_final_jeopardy = ClueExtractor.extract_fj_question(contestant_names, game_id, clues.filter(_.isRight).length, _: Element)

    val invalid_extractions = clues.filter(_.isLeft)

    val fj = extract_final_jeopardy(doc >> element("table.final_round"))

    val all_regular_are_valid = (invalid_extractions.length == 0 || 
                                 invalid_extractions.map(_.left.get.string).forall(_ == "Cannot extract mouse over"))

    if(all_regular_are_valid && fj.isRight) {

        val valid_extractions = clues.filter(_.isRight).map(_.right.get)

        val n_single_jeopardy = valid_extractions.filter{case (lt: List[Turn], q: Question) => !q.question_id.contains("DJ")}.length

        val update_clue_number = Utility.update_clue_order_number(n_single_jeopardy)(_, _)

        val extractions_updated_number = valid_extractions.map{case (lt: List[Turn], q: Question) => update_clue_number(lt, q)}
                                                
        val fj_turns = fj.right.get._1
        val fj_question = fj.right.get._2

        val turns = extractions_updated_number.flatMap(_._1)
        val questions = extractions_updated_number.map(_._2)
        
        unpacked_contestants.map(insert_contestants)
        insert_date(Date(game_id, args(3)))

        questions.map(insert_question)
        turns.map(insert_turn)
        insert_question(fj_question)
        fj_turns.map(insert_turn)

       }
    else {
        throw new RuntimeException(" One invalid turn or question was found")
    }
    
}