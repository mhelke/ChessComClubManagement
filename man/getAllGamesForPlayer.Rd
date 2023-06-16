\name{getAllGamesForPlayer}
\alias{getGameStatsForPlayer}
\title{Get the stats on the timeouts for a player}
\usage{
getAllGamesForPlayer(user_id, year, month, nmonths)
}
\description{
Fetches a player's daily games from the archive for n months. Returns all the information for the game as given by the chess.com public API
}
\examples{
getAllGamesForPlayer(user_id = "MyUsername", year = 2023, month = 5, nmonths = 2)
}
