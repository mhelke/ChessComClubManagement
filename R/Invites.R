#' @import dplyr
#' @import cli
#' @importFrom httr GET
#' @importFrom stringr str_split_1
#' @importFrom lubridate as_datetime ymd

#' @name getUsersToInvite
#' @title Get a list of users to invite to your club
#' @description Get a list of ideal players within a given club based on provided parameters
#' @param club_id ID of the club you want to invite members from
#' @param max_timeout The maximum allowed timeout percentage required to participate
#' @param max_move_speed The maximum time per move required to be invited (in hours)
#' @param min_games The minimum number of games that must be completed before joining
#' @param min_months_account_age The minimum age of the user's account
#' @param min_days_last_online The number of days ago since the user was last online
#' @param min_rating The minimum allowed rating of the user
#' @param country_code The country code on the user's profile. Each country has a 2 character code assigned by chess.com
#' @returns Data frame of ideal players within a given club
#' @source Chess.com public API
#' @export
getUsersToInvite <- function(club_id,
                             max_timeout = NA,
                             max_move_speed = NA,
                             min_games = 0,
                             min_months_account_age = 0,
                             min_days_last_online = NA,
                             min_rating = 0,
                             country_code = NA) {
  # Verify given data is accurate
  if (nchar(country_code) != 2 & !is.na(country_code)) {
    cli_abort(
      c("Invalid country code: `{country_code}`.",
        "i" = "Please find the correct country code here: https://www.chess.com/news/view/published-data-api#pubapi-endpoint-country"
      )
    )
  }

  # Fetch club members
  invites <- getAllMemberStats(club_id)

  # Process the dates needed for calculations
  invites <- invites %>%
    mutate(last_online = as.POSIXct(last_online, origin = "1970-01-01", tz = "UTC")) %>%
    mutate(joined_site = as.POSIXct(joined_site, origin = "1970-01-01", tz = "UTC"))

  # Get a list of users who meet the criteria

  # Timeouts
  if (!is.na(max_timeout)) {
    start <- count(invites)
    invites <- invites %>%
      filter(timeout_percent <= max_timeout)

    change <- start - count(invites)
    cli_alert_info("Dropped {change} players on max timeout requirement of {max_timeout}")
  }

  # Move speed
  if (!is.na(max_move_speed)) {
    start <- count(invites)
    invites <- invites %>%
      filter(time_per_move <= max_move_speed)

    change <- start - count(invites)
    cli_alert_info("Dropped {change} players on max move speed requirement of {max_move_speed}")
  }

  # Last online date
  if (!is.na(min_days_last_online)) {
    start <- count(invites)
    invites <- invites %>%
      filter(last_online >= ymd(Sys.Date()) - days(min_days_last_online))

    change <- start - count(invites)
    cli_alert_info(
      "Dropped {change} players on min days last online requirement of {min_days_last_online}"
    )
  }

  # Country code
  if (!is.na(country_code)) {
    start <- count(invites)
    invites <- invites %>%
      filter(grepl(country_code, str_sub(country, -2, -1), ignore.case = TRUE))

    change <- start - count(invites)
    cli_alert_info("Dropped {change} players on country requirement of {country_code}")
  }

  # Completed games
  start <- count(invites)
  invites <- invites %>%
    filter(total_games >= min_games)

  change <- start - count(invites)
  cli_alert_info("Dropped {change} players onmin games played requirement of {min_games}")

  # Min rating
  start <- count(invites)
  invites <- invites %>%
    filter(daily_rating >= min_rating)

  change <- start - count(invites)
  cli_alert_info("Dropped {change} players on min rating requirement of {min_rating}")

  # Account age
  start <- count(invites)
  invites <- invites %>%
    filter(joined_site <= ymd(Sys.Date()) - months(min_months_account_age))

  change <- start - count(invites)
  cli_alert_info("Dropped {change} players on min account age requirement of {min_months_account_age}")

  final_count <- count(invites)
  if (final_count == 0) {
    cli_warn(
      c("No players meet the given criteria!", "i" = "Try modifying the parameters and try again")
    )
  } else {
    cli_alert_success("Returning {final_count} players that meet the given criteria")
  }
  return(invites)
}
