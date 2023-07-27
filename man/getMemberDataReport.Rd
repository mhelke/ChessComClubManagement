\name{getMemberDataReport}
\alias{getMemberDataReport}
\title{Get all member data}
\usage{
getMemberDataReport(club_id, include_finished_matches,include_in_progress_matches, include_upcoming_matches, nDays = NA, convert_country = FALSE)
}
\description{
Fetches stats and team match performance data for all members of a club and returns a full, clean report.
}
\examples{
getMemberDataReport(club_id, include_finished_matches = TRUE,include_in_progress_matches = TRUE, include_upcoming_matches = FALSE, nDays = 90, convert_country = TRUE)
getMemberDataReport(club_id, include_finished_matches = TRUE,include_in_progress_matches = FALSE, include_upcoming_matches = FALSE)
}
