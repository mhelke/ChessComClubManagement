\name{getMatchIds}
\alias{getMatchIds}
\title{Get the Daily Match Ids for a given club}
\usage{
getMatchIds(clubId, include_finished = TRUE, include_in_progress = TRUE, include_upcoming = TRUE, nDays = NA)
}
\description{
Fetches the daily match Ids for the given club and returns a list of Ids. You can specify which match status to include (finished, in-progress, and upcoming) as well as the number of days in the past the match started. A value of nDays = NA will return all matches that are available from the chess.com API.
}
\examples{
getMatchIds(clubId = "my-club")
getMatchIds(clubId = "my-club", include_finished = FALSE, include_in_progress = FALSE, include_upcoming = TRUE, nDays = 90)
getMatchIds(clubId = "my-club", include_finished = FALSE, include_in_progress = FALSE, include_upcoming = TRUE)
}
