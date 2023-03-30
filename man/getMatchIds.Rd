\name{getMatchIds}
\alias{getMatchIds}
\title{Get the Match Ids for a given club}
\usage{
getMatchIds(clubId, include_finished = TRUE, include_in_progress = TRUE, include_upcoming = TRUE)
}
\description{
Fetches the match Ids for the given club and returns a list of Ids. You can specify which match status to include (finished, in-progress, and upcoming).
}
\examples{
getMatchIds(clubId = "my-club")
getMatchIds(clubId = "my-club", include_finished = FALSE, include_in_progress = FALSE, include_upcoming = TRUE)
}
