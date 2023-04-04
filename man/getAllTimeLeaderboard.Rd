\name{getAllTimeLeaderboard}
\alias{getAllTimeLeaderboard}
\title{Get All Time Leaderboard for Team Matches}
\usage{
getAllTimeLeaderboard(clubId)
}
\description{
Fetches the record of match results for all players who have ever played a match for the club.
Note: chess.com is temporarily limiting finished matches to the most recent 500 due to performance.
}
\examples{
getAllTimeLeaderboard(clubId = "my-club")
}
