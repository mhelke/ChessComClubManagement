\name{getMatchUrls}
\alias{getMatchUrls}
\title{Get the API URLs of matches for a given club}
\usage{
getMatchUrls(clubId, include_finished = TRUE, include_in_progress = TRUE, include_upcoming = TRUE)
}
\description{
Fetches the URLs for the given club and returns a list of URLs. You can specify which match status to include (finished, in-progress, and upcoming).
}
\examples{
getMatchUrls(clubId = "my-club")
getMatchUrls(clubId = "my-club", include_finished = FALSE, include_in_progress = FALSE, include_upcoming = TRUE)
}
