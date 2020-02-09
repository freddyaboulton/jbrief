package jbrief

final case class Contestant(contestant_id: Int, first_name: String, last_name: String,
                            hometown: String, occupation: String)

final case class Question(game_id: Int, question_id: String, text: String, answer: String,
                          is_daily_double: Boolean, is_final_jeopardy: Boolean)

final case class Turn(game_id: Int, contestant_id: Int, question_id: String,
                      clue_order_number: Int, change_in_score: Float)