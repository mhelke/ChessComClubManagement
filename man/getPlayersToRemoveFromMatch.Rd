\name{getPlayersToRemoveFromMatch}
\alias{getPlayersToRemoveFromMatch}
\title{Get a list of users ineligible to participate in a given match}
\usage{
getPlayersToRemoveFromMatch(match_id, club_id, max_timeouts, min_total_games)
}
\description{
Returns ineligible players for a given match based on the provided criteria.
}
\examples{
getPlayersToRemoveFromMatch(match_id = 100, club_id = "my-club", max_timeouts = 25, min_total_games = 10)
}
