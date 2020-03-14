package jbrief.db

import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts
import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import java.sql.Connection

object CreateDB extends App {

    // We need a ContextShift[IO] before we can construct a Transactor[IO]. The passed ExecutionContext
    // is where nonblocking operations will be executed. For testing here we're using a synchronous EC.
    implicit val cs = IO.contextShift(ExecutionContexts.synchronous)

    // A transactor that gets connections from java.sql.DriverManager and executes blocking operations
    // on an our synchronous EC. See the chapter on connection handling for more info.
    val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", 
    "jdbc:postgresql:jbrief",
    "jbriefwriter",
    "jbrief",
    )

    def drop(table_name: String) = sql"DROP TABLE IF EXISTS {$table_name}"

    val createConstestant: ConnectionIO[Int] = 
    sql"""
    CREATE TABLE contestant (
        contestant_id INTEGER PRIMARY KEY,
        first_name VARCHAR NOT NULL,
        last_name VARCHAR NOT NULL,
        hometown VARCHAR NOT NULL,
        occupation VARCHAR NOT NULL
    );
    """.update.run

    val createQuestion: ConnectionIO[Int] = 
    sql"""
    CREATE TABLE question (
        game_id INTEGER NOT NULL,
        question_id VARCHAR NOT NULL,
        text VARCHAR NOT NULL,
        answer VARCHAR NOT NULL,
        is_dd BOOLEAN NOT NULL,
        is_fj BOOLEAN NOT NULL,
        PRIMARY KEY (game_id, question_id),
        UNIQUE (game_id, question_id)
    );
    """.update.run

    val createGame: ConnectionIO[Int] = 
    sql"""
    CREATE TABLE game (
        id SERIAL PRIMARY KEY,
        game_id INTEGER,
        question_id VARCHAR,
        contestant_id INTEGER REFERENCES contestant (contestant_id),
        clue_order_number INTEGER NOT NULL,
        change_in_value REAL NOT NULL,
        FOREIGN KEY (game_id, question_id) REFERENCES question (game_id, question_id)
        );
    """.update.run

    val CreateDate: ConnectionIO[Int] = 
    sql"""
    CREATE TABLE date(
        date VARCHAR PRIMARY KEY,
        game_id INTEGER UNIQUE
    );
    """.update.run

    sql"""DROP TABLE IF EXISTS contestant CASCADE""".update.run.transact(xa).unsafeRunSync
    sql"""DROP TABLE IF EXISTS question CASCADE""".update.run.transact(xa).unsafeRunSync
    sql"""DROP TABLE IF EXISTS game CASCADE""".update.run.transact(xa).unsafeRunSync

    
    createConstestant.transact(xa).unsafeRunSync
    createQuestion.transact(xa).unsafeRunSync
    createGame.transact(xa).unsafeRunSync
}

