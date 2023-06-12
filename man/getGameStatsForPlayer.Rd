\name{getGameStatsForPlayer}
\alias{getGameStatsForPlayer}
\title{Get the stats on the timeouts for a player}
\usage{
getGameStatsForPlayer(user_id, year, month, nmonths)
}
\description{
Query a player's game archive to find all match and tournament games. Returns athe number of daily game for each grouping of event type, time control, and result.
The year and month parameters refer to the month to start with and nmonths is how many total months to query.
}
\examples{
getGameStatsForPlayer(user_id = "MyUsername", year = 2023, month = 5, nmonths = 2)
}
