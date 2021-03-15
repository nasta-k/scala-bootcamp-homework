package json


import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import io.circe
import io.circe.Decoder
import io.circe.parser._
import io.circe.generic.JsonCodec
import io.circe.generic.extras.{Configuration, ConfiguredJsonCodec, JsonKey}
import scalaj.http.Http

object Json {
  implicit val config: Configuration = Configuration.default.withDefaults

  @ConfiguredJsonCodec final case class TeamTotals(
                                                    assists: String,
                                                    @JsonKey("full_timeout_remaining") fullTimeoutRemaining: String,
                                                    plusMinus: String)

  @JsonCodec final case class TeamBoxScore(totals: TeamTotals)

  @JsonCodec final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore)

  implicit val decodeLocalDate: Decoder[LocalDate] =
    Decoder.decodeLocalDateWithFormatter(DateTimeFormatter.BASIC_ISO_DATE)

  @JsonCodec final case class PrevMatchup(gameDate: LocalDate, gameId: String)

  @JsonCodec final case class BoxScore(
                                        basicGameData: Game,
                                        previousMatchup: PrevMatchup,
                                        stats: Option[GameStats],
                                      )

  @JsonCodec final case class JustScore(score: String)

  @JsonCodec final case class TeamStats(
                                         linescore: List[JustScore],
                                         loss: String,
                                         score: String,
                                         teamId: String,
                                         triCode: String
                                       )

  @JsonCodec final case class GameDuration(hours: String, minutes: String)

  @JsonCodec final case class Arena(
                                     city: String,
                                     country: String,
                                     isDomestic: Boolean,
                                     name: String,
                                     stateAbbr: String
                                   )

  @JsonCodec final case class Game(
                                    arena: Arena,
                                    attendance: String,
                                    endTimeUTC: Option[ZonedDateTime],
                                    gameDuration: GameDuration,
                                    gameId: String,
                                    gameUrlCode: String,
                                    hTeam: TeamStats,
                                    isBuzzerBeater: Boolean,
                                    startTimeUTC: ZonedDateTime,
                                    vTeam: TeamStats,
                                  )

  @JsonCodec final case class Scoreboard(games: List[Game], numGames: Int)

  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json").asString.body
    decode[Scoreboard](body)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json").asString.body
    decode[BoxScore](body)
  }
}
