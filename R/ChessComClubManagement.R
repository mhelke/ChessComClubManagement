##############################################
###         Required Imports              ####
### Must be installed to use this package ####
##############################################

require(tidyverse)
require(jsonlite)
require(dplyr)

########################
### MATCH MANAGEMENT ###
########################

#' @description Collects the details of the specified team matches into a tibble
#' @param club_id ID of the club you want matches details for
#' @param match_ids List of IDs of the matches you want details for
#' @return Tibble of all specified match data
#' @source chess.com public API
#' @export
getMatchDetailsForMatches <- function(club_id, match_ids) {
  match_details <- data.frame(
    username = character(),
    played_as_white = character(),
    played_as_black = character(),
    board = character(),
    time_out_count = character()
  )

  i <- 1
  while(i <= length(match_ids)) {
    print(paste0(i, "/", length(match_ids), ": Fetching match: ", match_ids[i]))
    details <- .getMatchDetails(club_id, match_ids[i])
    match_details <- match_details %>% rbind(details)
    i <- i+1
  }
  print(paste("Finished fetching details for", length(match_ids), "matches"))
  return(match_details)
}

#' @description Retrieves IDs of matches for the given club
#' @param club_id ID of the club you want match IDs for
#' @param include_finished Returns finished match IDs
#' @param include_in_progress Returns in-progress match IDs
#' @param include_upcoming Returns upcoming match IDs
#' @return List of match IDs
#' @source chess.com public API
#' @export
getMatchIds <- function(club_id, include_finished = TRUE, include_in_progress = TRUE, include_upcoming = TRUE, nDays = NA) {
  all_matches <- getMatchUrls(club_id, include_finished, include_in_progress, include_upcoming, nDays)

  match_ids <- vector(mode="character", length = length(all_matches)-2)

  i <- 2
  while(i < length(all_matches)) {
    match <- all_matches[[i]]
    url_elements <- match %>% str_split_1('/')
    match_id <- url_elements %>% last()
    match_ids[i-1] <- match_id
    i <- i+1
  }
  print(paste("Finished fetching", length(match_ids), "matches"))
  return(match_ids)
}

#' @description Retrieves the URLs of matches for the given club
#' @note Use `getMatchIds` to retrieve only the IDs of the match.
#' @param club_id ID of the club you want match URLs for
#' @param include_finished Returns finished match URLs
#' @param include_in_progress Returns in-progress match URLs
#' @param include_upcoming Returns upcoming match URLs
#' @return List of match URLs
#' @source chess.com public API
#' @export
getMatchUrls <- function(club_id, include_finished = TRUE, include_in_progress = TRUE, include_upcoming = TRUE, nDays = NA) {
  message <- ifelse(is.na(nDays), "all time", paste0("the past ", nDays, " days"))
  print(paste0("Fetching team match URLs from ", message))
  baseUrl <- "https://api.chess.com/pub/club/"
  endpoint <- paste0(baseUrl, club_id, "/matches", sep = "", collapse = NULL)

  club_matches_raw <- try(fromJSON(toString(endpoint), flatten = TRUE))

  if(class(club_matches_raw) == "try-error") {
    stop("Error: matches cannot be found")
  }

  if(include_finished) {
    print("Including finished matches...")

    if(!is.na(nDays)) {
      current_time <- as.numeric(as.POSIXct(Sys.time()))
      one_day <- 86400
      days_ago <- current_time - nDays*one_day

      finished_matches <- club_matches_raw$finished %>% filter(start_time > days_ago)
    } else {
      finished_matches <- club_matches_raw$finished
    }

    finished_matches <- finished_matches %>%
      filter(time_class == "daily")
    finished_matches <- finished_matches$`@id` %>% as.list()

    matches <- append(matches, finished_matches)
  }
  if(include_upcoming) {
    print("Including upcoming matches...")

    upcoming_matches <- club_matches_raw$registered %>%
      filter(time_class == "daily")
    upcoming_matches <- upcoming_matches$`@id` %>% as.list()

    matches <- append(matches, upcoming_matches)
  }
  if(include_in_progress) {
    print("Including in-progress matches...")

    in_progress_matches <- club_matches_raw$in_progress %>%
      filter(time_class == "daily")
    in_progress_matches <- in_progress_matches$`@id` %>% as.list()

    matches <- append(matches, in_progress_matches)
  }
  return(matches)
}

#' @description Retrieves the all time stats from team matches and creates a leader board
#' @param club_id ID of the club you want the leader board for
#' @return A Tibble of club members who have participated in matches and their all-time records
#' @note Chess.com public API only returns the most recent 500 completed matches due to performance issues. Until that is resolved by the chess.com team, this function may NOT return the all-time stats and results may not be accurate for all clubs.
#' @source chess.com public API
#' @export
getAllTimeLeaderBoard <- function(club_id) {

  # Match IDs for all daily events
  match_ids <- getMatchIds(club_id, include_finished = TRUE, include_in_progress = TRUE, include_upcoming = FALSE)

  all_time_match_results <- getMatchDetailsForMatches(club_id, match_ids) %>%
    mutate(wins = if_else(played_as_white == 1, 1, 0, 0)) %>%
    mutate(wins = if_else(played_as_black == 1, wins+1, wins, wins)) %>%
    mutate(draws = if_else(played_as_white == .5, 1, 0, 0)) %>%
    mutate(draws = if_else(played_as_black == .5, draws+1, draws, draws)) %>%
    mutate(losses = if_else(played_as_white == 0, 1, 0, 0)) %>%
    mutate(losses = if_else(played_as_black == 0, losses+1, losses, losses)) %>%
    mutate(played_as_white = if_else(is.na(played_as_white), 0, played_as_white)) %>%
    mutate(played_as_black = if_else(is.na(played_as_black), 0, played_as_black)) %>%
    group_by(username) %>%
    summarise(
      games = sum(wins) + sum(draws) + sum(losses),
      score = sum(wins) + (sum(draws)/2),
      wins = sum(wins),
      draws = sum(draws),
      losses = sum(losses)
    )

  return(all_time_match_results)
}

#' @description Retrieves the record of the given club against all clubs played in team matches
#' @param club_id ID of the club you want the results for
#' @return A Tibble of the club's team match record versus other clubs
#' @note Chess.com public API only returns the most recent 500 completed matches due to performance issues. Until that is resolved by the chess.com team, this function may NOT return all the clubs ever played.
#' @source chess.com public API
#' @export
getMatchResults <- function(club_id) {
  baseUrl <- "https://api.chess.com/pub/club/"
  endpoint <- paste0(baseUrl, club_id, "/matches", sep = "", collapse = NULL)

  matches_raw <- try(fromJSON(toString(endpoint), flatten = TRUE), silent = TRUE)

  # Sometimes aborted matches are included from the API. Ignore these.
  if(class(matches_raw) == "try-error") {
    stop("Error: matches cannot be found")
  }

  matches <- matches_raw$finished %>%
    filter(time_class == "daily") %>%
    select(opponent, result) %>%
    mutate(opponent = sapply(opponent, .getId)) %>%
    mutate(wins = if_else(result == "win", 1, 0)) %>%
    mutate(draws = if_else(result == "draw", 1, 0)) %>%
    mutate(losses = if_else(result == "lose", 1, 0)) %>%
    group_by(opponent) %>%
    summarise(
      matches_played = n(),
      wins = sum(wins),
      draws = sum(draws),
      losses = sum(losses)
    ) %>%
    arrange(desc(matches_played))

  return(matches)

}

##########################
### MEMEBER MANAGEMENT ###
##########################

#' @description Retrieves all members of a given club
#' @param club_id ID of the club you want the list of members for
#' @return A list of members grouped by activity level (weekly, monthly,  all-time (inactive))
#' @seealso `getAllClubMembers` which returns the same data already merged into one table
#' @source chess.com public API
#' @export
getAllMembersByActivity <- function(club_id) {
  print(paste("Fetching members for club:", club_id))
  baseUrl <- "https://api.chess.com/pub/club/"
  endpoint <- paste0(baseUrl, club_id, "/members", sep = "", collapse = NULL)
  member_activity_raw <- try(fromJSON(toString(endpoint), flatten = TRUE))

  if(class(member_activity_raw) == "try-error") {
    stop("Error: members cannot be found")
  }

  return(member_activity_raw)
}

#' @description Retrieves all members of a given club
#' @param club_id ID of the club you want the members of
#' @return A Tibble of all members in the club and their join date
#' @source chess.com public API
#' @export
getAllClubMembers <- function(club_id) {
  all_members_by_activity <- getAllMembersByActivity(club_id)
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

  all_club_members <- rbind(weekly_members, monthly_members, all_time_members) %>%
    as.data.frame()

  return(all_club_members)
}

#' @description Calculates which members have not joined a team match in the past 90 days
#' @param club_id ID of the club you want the list of inactive members for
#' @return Tibble of members who have not joined a match in 90 days along with the date they joined the club
#' @source chess.com public API
#' @export
getInactiveMatchPlayers <- function(club_id) {
  # Get all match Ids
  all_match_ids <- getMatchIds(club_id)

  all_match_details_raw <- getMatchDetailsForMatches(club_id, all_match_ids)

  all_players_in_matches <- all_match_details_raw %>%
    select(username) %>%
    distinct()

  all_club_members <- getAllClubMembers(club_id)

  no_matches_past_90_days <- all_club_members %>%
    anti_join(all_players_in_matches, by = "username") %>%
    arrange(joined) %>%
    mutate(joined = format(as.POSIXct(joined, origin = "1970-01-01", tz = "UTC"), "%m/%d/%Y"))

  return(no_matches_past_90_days)
}

#' @description Returns relevant stats for a user
#' @param user_id ID of the user you want stats for
#' @return One row tibble of relevant user stats for club management: Username, joined chess.com date, last online date, country, daily standard and 960 ratings, time per move, and timeout percent
#' @source chess.com public API
#' @export
getUserStats <- function(user_id) {
  baseUrl <- "https://api.chess.com/pub/player/"
  endpoint <- paste0(baseUrl, user_id, sep = "", collapse = NULL)
  user_profile <- try(fromJSON(toString(endpoint), flatten = TRUE)) # raw data of member activity (username, join date)

  if(class(user_profile) == "try-error") {
    stop(paste("Error: user ", user_id, " cannot be found", sep = "", collapse = NULL))
  }

  user_profile <- as.data.frame(user_profile)

  baseUrl <- "https://api.chess.com/pub/player/"
  endpoint <- paste0(baseUrl, user_id, "/stats", sep = "", collapse = NULL)
  user_stats_raw <- try(fromJSON(toString(endpoint), simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))

  if(class(user_stats_raw) == "try-error") {
    stop(paste("Error: stats for user ", user_id, " cannot be found", sep = "", collapse = NULL))
  }

  user_stats_unlisted <- unlist(user_stats_raw , use.names = TRUE)

  user_stats <- as.data.frame(t(user_stats_unlisted))
  seconds_in_hour <- 60*60

  user_stats <- .add_cols(
    user_stats,
    c("chess_daily.last.rating",
      "chess960_daily.last.rating",
      "chess_daily.record.time_per_move",
      "chess_daily.record.timeout_percent",
      "chess_daily.record.win",
      "chess_daily.record.loss",
      "chess_daily.record.draw"
      ))

  user_clean_stats <- user_stats %>%
    select(chess_daily.last.rating, chess960_daily.last.rating, chess_daily.record.time_per_move, chess_daily.record.timeout_percent,
           chess_daily.record.win, chess_daily.record.loss, chess_daily.record.draw) %>%
    mutate(username = user_id) %>%
    mutate(across(c(chess_daily.last.rating, chess960_daily.last.rating, chess_daily.record.time_per_move, chess_daily.record.timeout_percent,
                    chess_daily.record.win, chess_daily.record.loss, chess_daily.record.draw), as.numeric)) %>%
    rename(daily_rating = chess_daily.last.rating) %>%
    rename(daily_960_rating = chess960_daily.last.rating) %>%
    rename(time_per_move = chess_daily.record.time_per_move) %>%
    rename(timeout_percent = chess_daily.record.timeout_percent) %>%
    mutate(time_per_move = time_per_move/(seconds_in_hour)) %>%
    inner_join(user_profile, by = "username") %>%
    rename(joined_site = joined) %>%
    mutate(total_games = sum(chess_daily.record.win, chess_daily.record.loss, chess_daily.record.draw)) %>%
    select(username, url, joined_site, last_online, country, daily_rating, daily_960_rating, time_per_move, timeout_percent, total_games)

  return(user_clean_stats)
}

#' @description Returns relevant stats for all club members
#' @param user_id ID of the club you want stats for
#' @return Tibble of relevant user stats for club management: Username, joined chess.com date, last online date, country, daily standard and 960 ratings, time per move, and timeout percent
#' @source chess.com public API
#' @export
getAllMemberStats <- function(club_id) {
  user_details <- data.frame(
    username = character(),
    url = character(),
    joined_club = numeric(),
    joined_site = numeric(),
    last_online = numeric(),
    country = character(),
    daily_rating  = numeric(),
    daily_960_rating  = numeric(),
    time_per_move  = numeric(),
    timeout_percent = numeric(),
    activity = character()
  )

  all_members <- getAllClubMembers(club_id)
  user_ids <- all_members$username

  i <- 1
  while(i <= length(user_ids)) {
    print(paste0(i, "/", length(user_ids), ": Fetching stats for user: ", user_ids[i]))
    details <- getUserStats(user_ids[i])
    user_details <- user_details %>% rbind(details)
    i <- i+1
  }

  user_details <- user_details %>%
    inner_join(all_members, by = "username") %>%
    select(username, url, joined_club, joined_site, last_online, activity, country, daily_rating, daily_960_rating, time_per_move, timeout_percent, total_games)

  return(user_details)
}

#' @description Calls the chess.com country API to get the country name. Provided only for convenience since all returns by default will not convert the country
#' @param countryEndpoint the URL for the chess.com country API
#' @return The name of the country
#' @source chess.com public API
#' @export
convertCountryCode <- function(countryEndpoint) {
  if(is.na(countryEndpoint)) {
    return(NA)
  }
  print(paste0("Converting country ", countryEndpoint))
  country_info <- try(fromJSON(toString(countryEndpoint), flatten = TRUE)) # raw data of member activity (username, join date)
  if(class(country_info) == "try-error") {
    warning("Failed to fetch country")
    return(NA)
  }
  return(country_info$name)
}

#########################
### Invite Management ###
#########################

#' @description Get a list of ideal players within a given club based on provided parameters
#' @param club_id ID of the club you want to invite members from
#' @param max_timeout The maximum allowed timeout percentage required to participate
#' @param max_move_speed The maximum time per move required to be invited (in hours)
#' @param min_games The minimum number of games that must be completed before joining
#' @param min_months_account_age The minimum age of the user's account
#' @param min_days_last_online The number of days ago since the user was last online
#' @param country The country on the user's profile
#' @return Data frame of ideal players within a given club
#' @export
getUsersToInvite <- function(club_id,
                             max_timeout = NA,
                             max_move_speed = NA,
                             min_games = 0,
                             min_months_account_age = 0,
                             min_days_last_online = NA,
                             country_code = NA) {

  # Fetch club members
  invites <- getAllMemberStats(club_id)

  # SAVE_THIS <- invites
  invites <- SAVE_THIS
  SAVE_THIS <- invites

  invites <- invites %>%
    mutate(last_online = as.POSIXct(last_online, origin = "1970-01-01", tz = "UTC")) %>%
    mutate(joined_site = as.POSIXct(joined_site, origin = "1970-01-01", tz = "UTC"))
  # Get a list of users who meet the criteria

  if(!is.na(max_timeout)) {
    invites <- invites %>%
      filter(timeout_percent <= max_timeout)
  }

  if(!is.na(max_move_speed)) {
    invites <- invites %>%
      filter(time_per_move <= max_timeout)
  }

  if(!is.na(min_days_last_online)) {
    invites <- invites %>%
      filter(last_online >= ymd(Sys.Date()) - days(min_days_last_online))
  }

  if(!is.na(country_code)) {
    invites <- invites %>%
      filter(grepl(country_code, str_sub(country, -2, -1), ignore.case = TRUE))
  }

  invites <- invites %>%
    filter(total_games >= min_games) %>%
    filter(joined_site <= ymd(Sys.Date()) - months(min_months_account_age))

  return(invites)
}


################################
### Private Helper Functions ###
################################

#' @description Collects the details of the specified team match into a tibble
#' @param club_id ID of the club you want match details for
#' @param match_id ID of the match you want details for
#' @note Use `getMatchDetailsForMatches` to fetch match details
#' @return Tibble of match data
#' @seealso `getMatchDetailsForMatches`
.getMatchDetails <- function(club_id, match_id) {
  empty_tibble <- tibble(
    username = character(),
    played_as_white = character(),
    played_as_black = character(),
    board = character(),
    time_out_count = character()
  )

  baseUrl <- "https://api.chess.com/pub/match/"
  endpoint <- paste0(baseUrl, match_id, sep = "", collapse = NULL)

  match_details_raw <- try(fromJSON(toString(endpoint), flatten = TRUE), silent = TRUE)

  # Sometimes aborted matches are included from the API. Ignore these.
  if(class(match_details_raw) == "try-error") {
    warning(paste("Match", match_id, "cannot be found"))
    return(empty_tibble)
  }

  team1 <- match_details_raw$teams$team1
  team2 <- match_details_raw$teams$team2

  found_matching_team <- FALSE

  if(grepl(club_id, team1$`@id`, fixed=TRUE)) {
    my_team <- team1
    found_matching_team <- TRUE
  }
  if(grepl(club_id, team2$`@id`, fixed=TRUE)) {
    my_team <- team2
    found_matching_team <- TRUE
  }

  if(found_matching_team) {
    # No players have registered yet, return an empty tibble
    if(!"players" %in% names(my_team)) {
      warning(paste("Could not find players for team", club_id, "for match", match_id))
      return(empty_tibble)
    }

    my_team_players <- my_team$players %>% as.data.frame()

    if(all(dim(my_team_players)) == 0) {
      warning(paste("No players registered for team", club_id, "for match", match_id))
      return(empty_tibble)
    }

    expected_col_names <- c("username", "stats", "timeout_percent", "status", "played_as_white", "played_as_black", "board")
    missing_col <- setdiff(expected_col_names, names(my_team_players))
    my_team_players[missing_col] <- "In progress"
    my_team_players <- my_team_players[expected_col_names]

    # Add column for timeout
    my_team_players <- my_team_players %>%
      mutate(time_out_count = if_else(played_as_white == "timeout", 1, 0, 0)) %>%
      mutate(time_out_count = if_else(played_as_black == "timeout", time_out_count+1, time_out_count, time_out_count))

    # Refactor results to be 0, 1, or 1/2
    my_team_players <- my_team_players %>%
      mutate(played_as_white = if_else(played_as_white %in% c("resigned", "checkmated", "timeout"), "0", played_as_white)) %>%
      mutate(played_as_white = if_else(played_as_white == "win", "1", played_as_white)) %>%
      mutate(played_as_white = if_else(is.na(played_as_white), "In progress", played_as_white)) %>%
      mutate(played_as_white = if_else(!played_as_white %in% c("In progress", "0", "1"),"0.5", played_as_white)) %>%
      mutate(played_as_black = if_else(played_as_black %in% c("resigned", "checkmated", "timeout"), "0", played_as_black)) %>%
      mutate(played_as_black = if_else(played_as_black == "win", "1", played_as_black)) %>%
      mutate(played_as_black = if_else(is.na(played_as_black), "In progress", played_as_black)) %>%
      mutate(played_as_black = if_else(!played_as_black %in% c("In progress", "0", "1"),"0.5", played_as_black)) %>%
      mutate(played_as_white = as.numeric(played_as_white)) %>%
      mutate(played_as_black = as.numeric(played_as_black))

    # Drop the link to player stats API and member status columns.
    my_team_players <- my_team_players %>%
      select(c(username, played_as_white, played_as_black, board, time_out_count))

  } else {
    my_team_players = empty_tibble
    warning(paste("Could not find team for", club_id, "for match", match_id))
  }
  return(my_team_players)
}

# Checks the data frame for the provided columns.If they do not exist, they're added and filled with NA values.
.add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if(length(add) != 0) df[add] <- NA
  return(df)
}

.getId <- function(url) {
  url_elements <- url %>% str_split_1('/')
  id <- url_elements %>% last()
  return(id)
}
