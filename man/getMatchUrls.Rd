\name{getMatchUrls}
\alias{getMatchUrls}
\title{Get the API URLs of matches for a given club}
\usage{
getMatchUrls(club_id, include_finished = TRUE, include_in_progress = TRUE, include_upcoming = TRUE, nDays = NA)
}
\description{
Fetches the URLs for the given club and returns a list of URLs. You can specify which match status to include (finished, in-progress, and upcoming) as well as the number of days in the past the match started. A value of nDays = NA will return all match URLs that are available from the chess.com API.
}
\examples{
getMatchUrls(club_id = "my-club")
getMatchUrls(club_id = "my-club", include_finished = FALSE, include_in_progress = FALSE, include_upcoming = TRUE, nDays = 90)
}
