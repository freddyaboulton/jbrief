package jbrief.db

import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts

import jbrief._
import cats.data.Validated.{Invalid, Valid}
import cats.effect._
import doobie.util.transactor

object Statements {

    def insertContestant(xa: transactor.Transactor.Aux[IO, Unit])(contestant: Contestant) = {
        val id = contestant.contestant_id
        val first_name = contestant.first_name
        val last_name = contestant.last_name
        val hometown = contestant.hometown
        val occupation = contestant.occupation
        sql"""INSERT INTO contestant (contestant_id,
                                      first_name,
                                      last_name,
                                      hometown,
                                      occupation)
                          values      ($id,
                                       $first_name,
                                       $last_name,
                                       $hometown,
                                       $occupation)
               ON CONFLICT DO NOTHING
            """.update.run.transact(xa).unsafeRunSync
    }
    
    def insertQuestion(xa: transactor.Transactor.Aux[IO, Unit])(question: Question) = {
        val game_id = question.game_id
        val question_id = question.question_id
        val text = question.text
        val answer = question.answer
        val is_daily_double = question.is_daily_double
        val is_final_jeopardy = question.is_final_jeopardy
        sql"""INSERT INTO question (question_id,
                                    game_id,
                                    text, 
                                    answer,
                                    is_dd,
                                    is_fj)
                          values   ($question_id,
                                    $game_id,
                                    $text,
                                    $answer,
                                    $is_daily_double,
                                    $is_final_jeopardy)
            """.update.run.transact(xa).unsafeRunSync
    }

    def insertTurn(xa: transactor.Transactor.Aux[IO, Unit])(turn: Turn) = {
        val game_id = turn.game_id
        val contestant_id = turn.contestant_id
        val question_id = turn.question_id
        val clue_order_number = turn.clue_order_number
        val change_in_score = turn.change_in_score
        sql"""INSERT INTO game (game_id,
                                question_id,
                                contestant_id,
                                clue_order_number,
                                change_in_value)
                         values ($game_id,
                                 $question_id,
                                 $contestant_id,
                                 $clue_order_number,
                                 $change_in_score)
        """.update.run.transact(xa).unsafeRunSync
    }

    def insertDate(xa: transactor.Transactor.Aux[IO, Unit])(date: Date) = {
        val game_id = date.game_id
        val date_of_game = date.date
        sql"""INSERT INTO date (date,
                                game_id)
                          values ($date_of_game,
                                  $game_id)
        """.update.run.transact(xa).unsafeRunSync
    }


}
