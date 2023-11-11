#' @import dplyr
#' @import cli
#' @importFrom httr GET
#' @importFrom stringr str_split_1
#' @importFrom lubridate as_datetime ymd

#' @name getAllMembersByActivity
#' @title Get All Club Members By Activity
#' @description Fetches all club members sorted by activity level (weekly, monthly, all-time (inactive) for the given club
#' @param club_id ID of the club you want the list of members for
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns A list of members grouped by activity level (weekly, monthly,  all-time (inactive))
#' @seealso [getAllClubMembers] which returns the same data already merged into one table
#' @source chess.com public API
#' @export
getAllMembersByActivity <- function(club_id, access_token = NA) {
  cli_alert_info("Fetching members for club `{club_id}`")
  baseUrl <- "https://api.chess.com/pub/club/"
  endpoint <-
    paste0(baseUrl,
           club_id,
           "/members",
           sep = "",
           collapse = NULL)
  member_activity_raw <- .fetch(endpoint, access_token)
  if (class(member_activity_raw) != "list") {
    cli_abort("Members for club `{club_id}` cannot be found")
  }

  cli_alert_success("Finished fetching members for club `{club_id}`")
  return(member_activity_raw)
}

#' @name getAllClubMembers
#' @title Get All Club Members
#' @description Fetches all club members for the given club and returns the usernames and dates they joined the club.
#' @param club_id ID of the club you want the members of
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns A Tibble of all members in the club and their join date
#' @source chess.com public API
#' @export
getAllClubMembers <- function(club_id, access_token = NA) {
  all_members_by_activity <-
    getAllMembersByActivity(club_id, access_token)
  weekly_members <- all_members_by_activity$weekly
  monthly_members <- all_members_by_activity$monthly
  all_time_members <- all_members_by_activity$all_time

  weekly_members <- weekly_members %>%
    mutate(activity = "Past week") %>%
    rename(joined_club = joined)

  monthly_members <- monthly_members %>%
    mutate(activity = "Past month") %>%
    rename(joined_club = joined)

  # "all_time" is the term used by the API for "inactive"
  all_time_members <- all_time_members %>%
    mutate(activity = "Inactive") %>%
    rename(joined_club = joined)

  all_club_members <-
    rbind(weekly_members, monthly_members, all_time_members) %>%
    as.data.frame() %>%

    mutate(joined_club = as_datetime(joined_club))

  return(all_club_members)
}

#' @name getInactiveMatchPlayers
#' @title Find players who have not played a match in the past n days
#' @description Calculates which members have not joined a team match in the past 90 days
#' @param club_id ID of the club you want the list of inactive members for
#' @param nDays The number of days the user has been inactive
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns Tibble of members who have not joined a match in the past n days along with the date they joined the club
#' @source chess.com public API
#' @export
getInactiveMatchPlayers <-
  function(club_id,
           nDays = 90,
           access_token = NA) {
    # Get all match Ids
    all_match_ids <- getMatchIds(club_id,
                                 nDays = nDays,
                                 access_token = access_token)

    all_match_details_raw <-
      getMatchDetailsForMatches(club_id, all_match_ids, access_token)

    all_players_in_matches <- all_match_details_raw %>%
      select(username) %>%
      distinct()

    all_club_members <- getAllClubMembers(club_id, access_token)

    no_matches_past_n_days <- all_club_members %>%
      anti_join(all_players_in_matches, by = "username") %>%
      arrange(joined) %>%
      mutate(joined = format(
        as.POSIXct(joined, origin = "1970-01-01", tz = "UTC"),
        "%m/%d/%Y"
      ))

    return(no_matches_past_n_days)
  }

#' @name getUserStats
#' @title Get Stats for a Given User
#' @description Returns relevant stats for a user
#' @param user_id ID of the user you want stats for
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns One row tibble of relevant user stats for club management: Username, joined chess.com date, last online date, country, daily standard and 960 ratings, time per move, and timeout percent
#' @source chess.com public API
#' @export
getUserStats <- function(user_id, access_token = NA) {
  baseUrl <- "https://api.chess.com/pub/player/"
  endpoint <- paste0(baseUrl, user_id, sep = "", collapse = NULL)

  # raw data of member activity (username, join date)
  user_profile <- .fetch(endpoint, access_token)
  if (class(user_profile) != "list") {
    cli_warn("User `{user_id}` cannot be found.")
    return(NA)
  }

  user_profile <-
    as.data.frame(user_profile) %>%
    .add_cols(cols = c("name"))

  baseUrl <- "https://api.chess.com/pub/player/"
  endpoint <-
    paste0(baseUrl,
           user_id,
           "/stats",
           sep = "",
           collapse = NULL)

  user_stats_raw <- .fetch(endpoint, access_token)
  if (class(user_stats_raw) != "list") {
    cli_warn("Stats for user `{user_id}` cannot be found")
    return(NA)
  }

  user_stats_unlisted <- unlist(user_stats_raw, use.names = TRUE)

  user_stats <- as.data.frame(t(user_stats_unlisted))
  seconds_in_hour <- 60 * 60

  user_stats <- .add_cols(
    user_stats,
    c(
      "chess_daily.last.rating",
      "chess960_daily.last.rating",
      "chess_daily.record.time_per_move",
      "chess_daily.record.timeout_percent",
      "chess_daily.record.win",
      "chess_daily.record.loss",
      "chess_daily.record.draw"
    )
  )

  user_clean_stats <- user_stats %>%
    select(
      chess_daily.last.rating,
      chess960_daily.last.rating,
      chess_daily.record.time_per_move,
      chess_daily.record.timeout_percent,
      chess_daily.record.win,
      chess_daily.record.loss,
      chess_daily.record.draw
    ) %>%
    mutate(username = user_id) %>%
    mutate(across(
      c(
        chess_daily.last.rating,
        chess960_daily.last.rating,
        chess_daily.record.time_per_move,
        chess_daily.record.timeout_percent,
        chess_daily.record.win,
        chess_daily.record.loss,
        chess_daily.record.draw
      ),
      as.numeric
    )) %>%
    rename(daily_rating = chess_daily.last.rating) %>%
    rename(daily_960_rating = chess960_daily.last.rating) %>%
    rename(time_per_move = chess_daily.record.time_per_move) %>%
    rename(timeout_percent = chess_daily.record.timeout_percent) %>%
    mutate(time_per_move = time_per_move / (seconds_in_hour)) %>%
    inner_join(user_profile, by = "username") %>%
    rename(joined_site = joined) %>%
    mutate(
      total_games = sum(
        chess_daily.record.win,
        chess_daily.record.loss,
        chess_daily.record.draw
      )
    ) %>%
    mutate(joined_site = as_datetime(joined_site)) %>%
    mutate(last_online = as_datetime(last_online)) %>%
    mutate(displayUsername = tail(unlist(strsplit(url, "/")), 1)) %>%
    select(
      username,
      url,
      displayUsername,
      name,
      joined_site,
      last_online,
      country,
      daily_rating,
      daily_960_rating,
      time_per_move,
      timeout_percent,
      total_games
    )
  return(user_clean_stats)
}

#' @name getAllMemberStats
#' @title Get Stats for all Users in the Given Club
#' @description Fetches relevant stats for all users in the given club.
#' @param club_id ID of the club you want user stats for
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns Tibble of relevant user stats for club management:
#' Username, joined chess.com date, last online date, country, daily standard and 960 ratings, time per move, and timeout percent
#' @source chess.com public API
#' @export
getAllMemberStats <- function(club_id, access_token = NA) {
  user_details <- data.frame(
    username = character(),
    url = character(),
    displayUsername = character(),
    name = character(),
    joined_club = numeric(),
    joined_site = numeric(),
    last_online = numeric(),
    country = character(),
    daily_rating = numeric(),
    daily_960_rating = numeric(),
    time_per_move = numeric(),
    timeout_percent = numeric(),
    activity = character()
  )

  column_names <- colnames(user_details)

  all_members <- getAllClubMembers(club_id, access_token)
  user_ids <- all_members$username

  total_users <- length(user_ids)
  cli_alert("Fetching stats for {total_users} users")
  cli_progress_bar("Fetching stats...", total = total_users)

  for (user_id in user_ids) {
    details <- getUserStats(user_id, access_token)
    if (class(details) == "data.frame") {
      user_details <- user_details %>% rbind(details)
    }
    cli_progress_update()
  }
  cli_progress_done()
  cli_alert_success("Finished fetching stats for {total_users} users")

  user_details <- user_details %>%
    inner_join(all_members, by = "username") %>%
    select(
      username,
      url,
      displayUsername,
      name,
      joined_club,
      joined_site,
      last_online,
      activity,
      country,
      daily_rating,
      daily_960_rating,
      time_per_move,
      timeout_percent,
      total_games
    )

  return(user_details)
}

#' @name convertCountryCode
#' @title Convert the Country Code from User Stats
#' @description Calls the chess.com country API to get the country name. Provided only for convenience since all returns by default will not convert the country.
#' @param countryEndpoint the URL for the chess.com country API
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns The name of the country
#' @source chess.com public API
#' @export
convertCountryCode <- function(countryEndpoint, access_token = NA) {
  if (is.na(countryEndpoint)) {
    return(NA)
  }
  cli_inform("Converting country `{countryEndpoint}`")

  country_info <- .fetch(countryEndpoint, access_token)
  if (class(country_info) != "list") {
    cli_warn("Failed to fetch tournament info")
    return(NA)
  }
  return(country_info$name)
}
