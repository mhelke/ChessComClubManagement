\name{getAllMembersByActivity}
\alias{getAllMembersByActivity}
\title{Get All Club Members By Activity}
\usage{
getAllMembersByActivity(club_id)
}
\description{
Fetches all club members sorted by activity level (weekly, monthly, all-time (in-active) for the given club and returns the a list of usernames and dates they joined the club grouped by activity level.
}
\examples{
getAllMembersByActivity(club_id = "my-club")
}
