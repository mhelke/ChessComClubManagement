\name{getUsersToInvite}
\alias{getUsersToInvite}
\title{Get a list of users to invite to your club}
\usage{
getUsersToInvite(club_id, max_timeout = NA, max_move_speed = NA, min_games = 0, min_months_account_age = 0, min_days_last_online = NA, country_code = NA)
}
\description{
Returns users to invite to your club based on the given criteria
}
\examples{
getUsersToInvite(club_id = "my-club")
getUsersToInvite(club_id, max_timeout = 10, max_move_speed = 12, min_games = 20, min_months_account_age = 3, min_days_last_online = 7, country_code = "US")
getUsersToInvite(club_id, max_timeout = 25, min_games = 10, min_days_last_online = 7)
}
