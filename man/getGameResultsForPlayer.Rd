\name{getGameResultsForPlayer}
\alias{getGameResultsForPlayer}
\title{Get the results and the timeouts of daily games for a player}
\usage{
getGameResultsForPlayer(user_id, year, month, nmonths)
}
\description{
Query a player's game archive to find all match and tournament games. Returns the number of daily game for each grouping of event type, time control, and result.
The year and month parameters refer to the month to start with and nmonths is how many total months to query.
}
\examples{
getGameResultsForPlayer(user_id = "MyUsername", year = 2023, month = 5, nmonths = 2)
}
