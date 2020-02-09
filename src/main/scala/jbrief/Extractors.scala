package jbrief

import net.ruippeixotog.scalascraper.model.Document
//import net.ruippeixotog.scalascraper.browser.JsoupElement
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import cats.data.Validated
import cats.implicits._
import jbrief.Errors.ParsingError
import cats.Monad

object Errors {
    sealed abstract class Error
    final case class ParsingError(string: String) extends Error
}

trait ParallelExtractor {
    type ExtractionResult[A] = Validated[List[ParsingError], A]
    type ValidatedExtractor[A, B] = (A => ExtractionResult[B])

    def validate[A, B](extractor: (A => B), message: String): ValidatedExtractor[A, B] = {
        (input: A) => Validated.catchNonFatal(extractor(input)).leftMap(_ => List(ParsingError(message)))
    }
}

object ContestantExtractor extends ParallelExtractor { 

    val get_name: ((Int, Element) => String) = (index, element) => {
        val name: String = element >> allText("a")
        name.split(" ")(index)
    }

    val extract_first_name: ValidatedExtractor[Element, String] = {
        
        validate(get_name(0, _), "cannot extract first name.")

    }

    val extract_last_name: ValidatedExtractor[Element, String] = 
        validate(get_name(1, _), "cannot extract last name.")

    val extract_hometown: ValidatedExtractor[Element, String] = 
        validate(_.text.split("from")(1).trim, "Cannot extract hometowns.")

    val extract_occupation: ValidatedExtractor[Element, String] = 
        validate(_.text.split("from")(0).split(",")(1).trim, "Cannot extract occupations")

    val extract_player_id: ValidatedExtractor[Element, Int] =
    {
        val extractor = (contestant: Element) => (contestant >> element("a")).attrs.getOrElse("href", "").split("=")(1).toInt
        validate(extractor, "Cannot extract player id.")
    }

    val extract_contestant: (Element => ExtractionResult[Contestant]) = {
        element => 
        (extract_player_id(element),
         extract_first_name(element),
         extract_last_name(element),
         extract_hometown(element),
         extract_occupation(element)).mapN(Contestant)
    }

    def extractContestants(elements: List[Element]) = elements.map(extract_contestant)
}

trait SequentialExtractor {
    type ExtractionResult[A] = Either[ParsingError, A]
    type MonadicExtractor[A, B] = (A => ExtractionResult[B])

    def toEither[A, B](extractor: (A => B), message: String): MonadicExtractor[A, B] = {
        (input: A) => try {
                Right(extractor(input))
        }
            catch {
                case _: Exception => Left(ParsingError(message))
            }
        }

}

object ClueExtractor extends SequentialExtractor {

    private val browser = JsoupBrowser()

    val convert_to_document: MonadicExtractor[String, Document] =
    toEither(s => browser.parseString(s), "Cannot convert String to Document")

    val extract_clue: MonadicExtractor[Element, Element] = 
    toEither(elem => elem >> element("td.clue_text"), "Cannot extract clue text.")

    val extract_clue_id: MonadicExtractor[Element, String] =
    toEither(elem => elem.attrs.get("id").get, "Cannot extract clue id")

    val extract_clue_text: MonadicExtractor[Element, String] =
    toEither(elem => elem.text, "Cannot extract clue text.")

    val extract_mouse_over: MonadicExtractor[Element, String] = 
    toEither(_ >> element("div") >> attr("onmouseover"), "Cannot extract mouse over")

    val extract_clue_header: MonadicExtractor[Element, Element] =
    toEither(elem => elem >> element("table.clue_header"), "Cannot extract clue header.")

    val extract_answer: MonadicExtractor[Document, String] =
    toEither(document => (document >> element("em.correct_response")).text, "Cannot extract answer.")

    val is_daily_double: MonadicExtractor[Element, Boolean] =
    toEither(elem => (elem >?> element("td.clue_value_daily_double")).nonEmpty, "Cannot extract if is daily double.")    
    
    val extract_value: MonadicExtractor[Element, Float] = {

        val extract_daily_double: (Element => Float) = (elem: Element) => {
            val text = (elem >> element("td.clue_value_daily_double")).text
            text.filter(_ != ',').slice(text.indexOf("$") + 1, text.length).toFloat    
        }

        val extract_text: (Element => Float) = (elem: Element) => {
            try {
                val text = (elem >> element("td.clue_value")).text
                text.tail.toFloat
            }
            catch {
                case e: NoSuchElementException => extract_daily_double(elem)
            }
        }

        toEither(extract_text, "Cannot extract clue value.")
    }
    
    val extract_clue_order_number: MonadicExtractor[Element, Int] =
    toEither(elem => (elem >> element("td.clue_order_number")).text.toInt,
    "Cannot extract clue order number.")

    val find_responders: (Document, String) => List[String] = {
        (doc: Document, with_response_type: String) => {
            val responders = doc >> element("tr") >> elementList("td." + with_response_type)
            responders.map(_.text).filter(_ != "Triple Stumper")
        }
    }
    
    val find_correct_responders: MonadicExtractor[Document, List[String]] =
    toEither(document => find_responders(document, "right"), "Cannot extract correct responders.")

    val find_wrong_responders: MonadicExtractor[Document, List[String]] =
    toEither(document => find_responders(document, "wrong"), "Cannot extract wrong responders.")

    val get_turns = (contestant_name: Map[String, Int],
                     game_id: Int,
                     question_id: String,
                     clue_order_number: Int,
                     value: Float,
                     correct_responders: List[String],
                     incorrect_responders: List[String]
                     ) => {
                     
                        val correct: List[Turn] = correct_responders.map((name: String) => contestant_name.getOrElse(name, 0))
                                                        .map((id: Int) => Turn(game_id, id, question_id, clue_order_number,
                                                                               value))
                        val incorrect: List[Turn] = incorrect_responders.map((name: String) => contestant_name.getOrElse(name, 0))
                                                            .map((id: Int) => Turn(game_id, id, question_id, clue_order_number,
                                                                                   -value))
                        correct ++ incorrect
                    }
    
    val extract_question: (Map[String, Int], Int, Element) => ExtractionResult[(List[Turn], Question)] = {
        (contestant_name: Map[String, Int], game_id: Int, element: Element) => for {
            mouse_over_text <- extract_mouse_over(element)
            mouse_over <- convert_to_document(mouse_over_text)
            clue <- extract_clue(element)
            clue_id <- extract_clue_id(clue)
            clue_text <- extract_clue_text(clue)
            answer <- extract_answer(mouse_over)
            header <- extract_clue_header(element)
            is_dd <- is_daily_double(header)
            value <- extract_value(header)
            correct_responders <- find_correct_responders(mouse_over)
            incorrect_responders <- find_wrong_responders(mouse_over)
            clue_order_number <- extract_clue_order_number(header)
            } yield (get_turns(contestant_name, game_id, clue_id,
                               clue_order_number, value, correct_responders,
                               incorrect_responders),
                    Question(game_id, clue_id, clue_text, answer, is_dd, false))
        }
    

}

