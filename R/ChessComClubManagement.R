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

  total_matches <- length(match_ids)
  i <- 1
  for (match in match_ids) {
    print(paste0(i, "/", total_matches, ": Fetching match: ", match))
    details <- .getMatchDetails(club_id, match)
    match_details <- match_details %>% rbind(details)
    i <- i + 1
  }
  message(paste("Finished fetching details for", total_matches, "matches"))
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
getMatchIds <-
  function(club_id,
           include_finished = TRUE,
           include_in_progress = TRUE,
           include_upcoming = TRUE,
           nDays = NA) {
    all_matches <-
      getMatchUrls(club_id,
                   include_finished,
                   include_in_progress,
                   include_upcoming,
                   nDays)

    total_matches <- length(all_matches)
    match_ids <-
      vector(mode = "character")

    for (match in all_matches) {
      url_elements <- match %>% str_split_1('/')
      match_id <- url_elements %>% last()
      match_ids <- append(match_ids, match_id)
    }
    message(paste("Finished fetching", length(match_ids), "matches"))
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
getMatchUrls <-
  function(club_id,
           include_finished = TRUE,
           include_in_progress = TRUE,
           include_upcoming = TRUE,
           nDays = NA) {
    message <-
      ifelse(is.na(nDays), "all time", paste0("the past ", nDays, " days"))
    message(paste0("Fetching team match URLs from ", message))
    baseUrl <- "https://api.chess.com/pub/club/"
    endpoint <-
      paste0(baseUrl,
             club_id,
             "/matches",
             sep = "",
             collapse = NULL)

    club_matches_raw <-
      tryCatch(
        fromJSON(toString(endpoint), flatten = TRUE),
        error = function(e) {
          stop("Error: matches cannot be found")
          print(e)
          return(NA)
        },
        warning = function(w) {
          stop("Error: matches cannot be found")
          print(w)
          return(NA)
        }
      )

    matches <- vector(mode = "character")
    if (include_finished) {
      message("Including finished matches...")

      if (!is.na(nDays)) {
        current_time <- as.numeric(as.POSIXct(Sys.time()))
        one_day <- 86400
        days_ago <- current_time - nDays * one_day

        finished_matches <-
          club_matches_raw$finished %>% filter(start_time > days_ago)
      } else {
        finished_matches <- club_matches_raw$finished
      }

      finished_matches <- finished_matches %>%
        filter(time_class == "daily")
      finished_matches <- finished_matches$`@id` %>% as.list()

      matches <- append(matches, finished_matches)
    }
    if (include_upcoming) {
      message("Including upcoming matches...")

      upcoming_matches <- club_matches_raw$registered %>%
        filter(time_class == "daily")
      upcoming_matches <- upcoming_matches$`@id` %>% as.list()

      matches <- append(matches, upcoming_matches)
    }
    if (include_in_progress) {
      message("Including in-progress matches...")

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
  match_ids <-
    getMatchIds(
      club_id,
      include_finished = TRUE,
      include_in_progress = TRUE,
      include_upcoming = FALSE
    )

  all_time_match_results <-
    getMatchDetailsForMatches(club_id, match_ids) %>%
    mutate(wins = if_else(played_as_white == 1, 1, 0, 0)) %>%
    mutate(wins = if_else(played_as_black == 1, wins + 1, wins, wins)) %>%
    mutate(draws = if_else(played_as_white == .5, 1, 0, 0)) %>%
    mutate(draws = if_else(played_as_black == .5, draws + 1, draws, draws)) %>%
    mutate(losses = if_else(played_as_white == 0, 1, 0, 0)) %>%
    mutate(losses = if_else(played_as_black == 0, losses + 1, losses, losses)) %>%
    mutate(played_as_white = if_else(is.na(played_as_white), 0, played_as_white)) %>%
    mutate(played_as_black = if_else(is.na(played_as_black), 0, played_as_black)) %>%
    group_by(username) %>%
    summarise(
      games = sum(wins) + sum(draws) + sum(losses),
      score = sum(wins) + (sum(draws) / 2),
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
  endpoint <-
    paste0(baseUrl,
           club_id,
           "/matches",
           sep = "",
           collapse = NULL)

  matches_raw <-
    tryCatch(
      fromJSON(toString(endpoint), flatten = TRUE),
      error = function(e) {
        stop("Error: matches cannot be found")
        print(e)
        return(NA)
      },
      warning = function(w) {
        stop("Error: matches cannot be found")
        print(w)
        return(NA)
      }
    )

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

#' @description Retrieves players registered for a match that are ineligible to participate
#' @param match_id ID of the match to manage
#' @param club_id ID of the club you manage
#' @param max_timeouts The maximum timeout percentage allowed for participation
#' @param min_total_games The minimum number of completed games allowed for participation
#' @return A list of players ineligible to participate in the match, filtered by the provided criteria
#' @source chess.com public API
#' @export
getPlayersToRemoveFromMatch <-
  function(match_id,
           club_id,
           max_timeouts,
           min_total_games) {
    if (is.na(match_id) |
        is.na(club_id) | is.na(max_timeouts) | is.na(min_total_games)) {
      stop("Error: Parameters cannot be NA")
    }

    match_details <- getMatchDetailsForMatches(club_id, match_id)

    players <- match_details$username

    user_details <- data.frame(
      username = character(),
      url = character(),
      joined_site = numeric(),
      last_online = numeric(),
      country = character(),
      daily_rating  = numeric(),
      daily_960_rating  = numeric(),
      time_per_move  = numeric(),
      timeout_percent = numeric(),
      activity = character(),
      total_games = numeric()
    )

    total_players <- length(players)

    if (total_players == 0) {
      stop(paste0(
        "Error: No players are signed up for match ",
        match_id,
        " on team ",
        club_id
      ))
    }

    i <- 1
    for (player in players) {
      print(paste0(i, "/", total_players, " Fetching stats for ", player))
      stats <- getUserStats(user_id = player)
      user_details <- user_details %>% rbind(stats)
      i <- i + 1
    }

    removals <- user_details %>%
      mutate(joined_site = as_datetime(joined_site)) %>%
      mutate(last_online = as_datetime(last_online)) %>%
      filter(timeout_percent >= max_timeouts |
               total_games < min_total_games) %>%
      select(
        username,
        url,
        joined_site,
        last_online,
        daily_rating,
        daily_960_rating,
        time_per_move,
        timeout_percent,
        total_games
      )


    return(removals)
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
  message(paste("Fetching members for club:", club_id))
  baseUrl <- "https://api.chess.com/pub/club/"
  endpoint <-
    paste0(baseUrl,
           club_id,
           "/members",
           sep = "",
           collapse = NULL)
  member_activity_raw <-
    tryCatch(
      fromJSON(toString(endpoint), flatten = TRUE),
      error = function(e) {
        stop("Error: members cannot be found")
        print(e)
        return(NA)
      },
      warning = function(w) {
        stop("Error: members cannot be found")
        print(w)
        return(NA)
      }
    )
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

  all_club_members <-
    rbind(weekly_members, monthly_members, all_time_members) %>%
    as.data.frame() %>%
    mutate(joined_club = as_datetime(joined_club))

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

  all_match_details_raw <-
    getMatchDetailsForMatches(club_id, all_match_ids)

  all_players_in_matches <- all_match_details_raw %>%
    select(username) %>%
    distinct()

  all_club_members <- getAllClubMembers(club_id)

  no_matches_past_90_days <- all_club_members %>%
    anti_join(all_players_in_matches, by = "username") %>%
    arrange(joined) %>%
    mutate(joined = format(
      as.POSIXct(joined, origin = "1970-01-01", tz = "UTC"),
      "%m/%d/%Y"
    ))

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

  # raw data of member activity (username, join date)
  user_profile <-
    tryCatch(
      fromJSON(toString(endpoint), flatten = TRUE),
      error = function(e) {
        warning(paste(
          "Error: user ",
          user_id,
          " cannot be found",
          sep = "",
          collapse = NULL
        ))
        print(e)
        return(NA)
      },
      warning = function(w) {
        warning(paste(
          "Warning: user ",
          user_id,
          " cannot be found",
          sep = "",
          collapse = NULL
        ))
        print(w)
        return(NA)
      }
    )
  user_profile <- as.data.frame(user_profile)

  baseUrl <- "https://api.chess.com/pub/player/"
  endpoint <-
    paste0(baseUrl,
           user_id,
           "/stats",
           sep = "",
           collapse = NULL)

  user_stats_raw <-
    tryCatch(
      fromJSON(
        toString(endpoint),
        simplifyVector = TRUE,
        simplifyDataFrame = TRUE,
        flatten = TRUE
      ),
      error = function(e) {
        warning(paste(
          "Error: stats for user ",
          user_id,
          " cannot be found",
          sep = "",
          collapse = NULL
        ))
        print(e)
        return(NA)
      },
      warning = function(w) {
        warning(paste(
          "Warning: stats for user ",
          user_id,
          " cannot be found",
          sep = "",
          collapse = NULL
        ))
        print(w)
        return(NA)
      }
    )

  user_stats_unlisted <- unlist(user_stats_raw , use.names = TRUE)

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
    select(
      username,
      url,
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

#' @description Returns relevant stats for all club members
#' @param user_id ID of the user you want stats for
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

  total_users <- length(user_ids)
  i <- 1
  for (user_id in user_ids) {
    print(paste0(i,
                 "/",
                 total_users,
                 ": Fetching stats for user: ",
                 user_id))
    details <- getUserStats(user_id)
    if (class(details) == "data.frame") {
      user_details <- user_details %>% rbind(details)
    }
    i <- i + 1
  }

  user_details <- user_details %>%
    inner_join(all_members, by = "username") %>%
    select(
      username,
      url,
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

#' @description Calls the chess.com country API to get the country name. Provided only for convenience since all returns by default will not convert the country
#' @param countryEndpoint the URL for the chess.com country API
#' @return The name of the country
#' @source chess.com public API
#' @export
convertCountryCode <- function(countryEndpoint) {
  if (is.na(countryEndpoint)) {
    return(NA)
  }
  print(paste0("Converting country ", countryEndpoint))

  # raw data of member activity (username, join date)
  country_info <- tryCatch(
    fromJSON(toString(countryEndpoint), flatten = TRUE),
    error = function(e) {
      warning("Failed to fetch country")
      print(e)
      return(NA)
    },
    warning = function(w) {
      warning("Failed to fetch country")
      print(w)
      return(NA)
    }
  )
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
#' @param country The country code on the user's profile. Each country has a 2 character code assigned by chess.com
#' @source Chess.com public API
#' @return Data frame of ideal players within a given club
#' @export
getUsersToInvite <- function(club_id,
                             max_timeout = NA,
                             max_move_speed = NA,
                             min_games = 0,
                             min_months_account_age = 0,
                             min_days_last_online = NA,
                             country_code = NA) {
  # Verify given data is accurate
  if (nchar(country_code) != 2 & !is.na(country_code)) {
    stop(
      paste0(
        "Error: Invalid country code: ",
        country_code,
        ". Please find the correct country code here:
               https://www.chess.com/news/view/published-data-api#pubapi-endpoint-country"
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
    message(paste0(
      "Dropped ",
      change,
      " players on max timeout requirement of ",
      max_timeout
    ))
  }

  # Move speed
  if (!is.na(max_move_speed)) {
    start <- count(invites)
    invites <- invites %>%
      filter(time_per_move <= max_move_speed)

    change <- start - count(invites)
    message(
      paste0(
        "Dropped ",
        change,
        " players on max move speed requirement of ",
        max_move_speed
      )
    )
  }

  # Last online date
  if (!is.na(min_days_last_online)) {
    start <- count(invites)
    invites <- invites %>%
      filter(last_online >= ymd(Sys.Date()) - days(min_days_last_online))

    change <- start - count(invites)
    message(
      paste0(
        "Dropped ",
        change,
        " players on min days last online requirement of ",
        min_days_last_online,
        " days"
      )
    )
  }

  # Country code
  if (!is.na(country_code)) {
    start <- count(invites)
    invites <- invites %>%
      filter(grepl(country_code, str_sub(country,-2,-1), ignore.case = TRUE))

    change <- start - count(invites)
    message(paste0(
      "Dropped ",
      change,
      " players on country requirement of ",
      country_code
    ))
  }

  # Completed games
  start <- count(invites)
  invites <- invites %>%
    filter(total_games >= min_games)

  change <- start - count(invites)
  message(
    paste0(
      "Dropped ",
      change,
      " players on minimum games played requirement of ",
      min_games
    )
  )

  # Account age
  start <- count(invites)
  invites <- invites %>%
    filter(joined_site <= ymd(Sys.Date()) - months(min_months_account_age))

  change <- start - count(invites)
  message(
    paste0(
      "Dropped ",
      change,
      " players on account age requirement of ",
      min_months_account_age,
      " months"
    )
  )

  final_count <- count(invites)
  if (final_count == 0) {
    warning("No players meet the given criteria!")
  } else {
    message(paste0(
      "Returning ",
      final_count,
      " players that meet the given criteria"
    ))
  }
  return(invites)
}

#' @description Returns the daily game results for a user in different event types and time controls
#' @param user_id ID of the user you want stats for
#' @param year Which year to start querying the user's archive from
#' @param month Which month to start querying the user's archive from
#' @param nmonths The number of months to query the archives
#' @return Tibble of the number of games for a user for each event type/time control/result grouping
#' @source chess.com public API
#' @export
getGameStatsForPlayer <- function(user_id, year, month, nmonths) {

  # TODO: Remove test code --
  user_id <- 'mastermatthew52'
  month <- 3
  year <- 2023
  nmonths = 4
  # ----

  baseUrl <- "https://api.chess.com/pub/player/"
  JSON_url <- vector(mode = "character", nmonths)

  # Fetches the data from the chess.com public API
  if (!is.null(nmonths) & nmonths != 1) {
    i <- 1
    while (nmonths != 0) {
      # Increment the year and provide the correct month
      if (month == 13) {
        month <- 1
        year <- year + 1
      }

      # A 0 is added to the month to create a double digit number, required by the API
      if (month < 10) {
        JSON_url[i] <-
          paste0(
            baseUrl,
            user_id,
            "/games/",
            year,
            "/0",
            month,
            sep = "",
            collapse = NULL
          )
      } else {
        JSON_url[i] <- paste0(
          baseUrl,
          user_id,
          "/games/",
          year,
          "/",
          month,
          sep = "",
          collapse = NULL
        )
      }

      month <- month + 1
      nmonths <- nmonths - 1
      i <- i + 1
    }
  } else {
    if (month < 10) {
      JSON_url[1] <-
        paste0(
          baseUrl,
          user_id,
          "/games/",
          year,
          "/0",
          month,
          sep = "",
          collapse = NULL
        )
    } else {
      JSON_url[1] <- paste0(
        baseUrl,
        user_id,
        "/games/",
        year,
        "/",
        month,
        sep = "",
        collapse = NULL
      )
    }
  }


  all_player_games <- data.frame()

  cols <-
    c(
      "url",
      "pgn",
      "time_control",
      "end_time",
      "rated",
      "tcn",
      "uuid",
      "initial_setup",
      "fen",
      "time_class",
      "rules",
      "start_time",
      "match",
      "tournament",
      "white.rating",
      "white.result",
      "white.@id",
      "white.username",
      "white.uuid",
      "black.rating",
      "black.result",
      "black.@id",
      "black.username",
      "black.uuid",
      "accuracies.white",
      "accuracies.black"
    )

  for (url in JSON_url) {
    player_games_raw <- tryCatch(
      fromJSON(toString(url), flatten = TRUE),
      error = function(e) {
        warning(paste("Games cannot be found"))
        print(e)
      },
      warning = function(w) {
        warning(paste("Games cannot be found"))
        print(w)
      }
    )

    player_games <- player_games_raw$games

    player_games <- .add_cols(player_games, cols)
    ncol(player_games)
    all_player_games <- all_player_games %>%
      rbind(player_games)
  }

  # Filter out daily games and opponent stats
  player_stats <- all_player_games %>%
    filter(time_class == "daily") %>%
    mutate(color = if_else(tolower(white.username) == tolower(user_id), "w", "b")) %>%
    mutate(username = if_else(color == "w", white.username, black.username)) %>%
    mutate(result = if_else(color == "w", white.result, black.result)) %>%
    select(username, result, time_control, match, tournament)

  # Filter for match/tournament timeouts
  results <- player_stats %>%
    mutate(event = if_else(!is.na(match), "match", "")) %>%
    mutate(event = if_else(!is.na(tournament), "tournament", event)) %>%
    mutate(
      result = if_else(
        result == "agreed" |
          result == "repetition" |
          result == "stalemate" |
          result == "insufficient" | result == "50move",
        "draw",
        result
      )
    ) %>%
    filter(event == "match" | event == "tournament") %>%
    mutate(time_control = substring(time_control, 3)) %>%
    mutate(time_control = as.numeric(time_control) / (60 * 60 * 24)) %>%
    group_by(time_control, event, result) %>%
    summarise(username = user_id,
              total_games = n())

  return(results)
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

  match_details_raw <- tryCatch(
    fromJSON(toString(endpoint), flatten = TRUE),
    error = function(e) {
      # Sometimes aborted matches are included from the API. Ignore these.
      warning(paste("Match", match_id, "cannot be found"))
      print(e)
      return(empty_tibble)
    },
    warning = function(w) {
      warning(paste("Match", match_id, "cannot be found"))
      print(w)
      return(empty_tibble)
    }
  )

  team1 <- match_details_raw$teams$team1
  team2 <- match_details_raw$teams$team2

  found_matching_team <- FALSE

  if (grepl(club_id, team1$`@id`, fixed = TRUE)) {
    my_team <- team1
    found_matching_team <- TRUE
  }
  if (grepl(club_id, team2$`@id`, fixed = TRUE)) {
    my_team <- team2
    found_matching_team <- TRUE
  }

  if (found_matching_team) {
    # No players have registered yet, return an empty tibble
    if (!"players" %in% names(my_team)) {
      warning(paste(
        "Could not find players for team",
        club_id,
        "for match",
        match_id
      ))
      return(empty_tibble)
    }

    my_team_players <- my_team$players %>% as.data.frame()

    if (all(dim(my_team_players)) == 0) {
      warning(paste(
        "No players registered for team",
        club_id,
        "for match",
        match_id
      ))
      return(empty_tibble)
    }

    expected_col_names <-
      c(
        "username",
        "stats",
        "timeout_percent",
        "status",
        "played_as_white",
        "played_as_black",
        "board"
      )
    missing_col <-
      setdiff(expected_col_names, names(my_team_players))
    my_team_players[missing_col] <- "In progress"
    my_team_players <- my_team_players[expected_col_names]

    # Add column for timeout
    my_team_players <- my_team_players %>%
      mutate(time_out_count = if_else(played_as_white == "timeout", 1, 0, 0)) %>%
      mutate(
        time_out_count = if_else(
          played_as_black == "timeout",
          time_out_count + 1,
          time_out_count,
          time_out_count
        )
      )

    # Refactor results to be 0, 1, or 1/2
    my_team_players <- my_team_players %>%
      mutate(played_as_white = if_else(
        played_as_white %in% c("resigned", "checkmated", "timeout"),
        "0",
        played_as_white
      )) %>%
      mutate(played_as_white = if_else(played_as_white == "win", "1", played_as_white)) %>%
      mutate(played_as_white = if_else(is.na(played_as_white), "In progress", played_as_white)) %>%
      mutate(played_as_white = if_else(
        !played_as_white %in% c("In progress", "0", "1"),
        "0.5",
        played_as_white
      )) %>%
      mutate(played_as_black = if_else(
        played_as_black %in% c("resigned", "checkmated", "timeout"),
        "0",
        played_as_black
      )) %>%
      mutate(played_as_black = if_else(played_as_black == "win", "1", played_as_black)) %>%
      mutate(played_as_black = if_else(is.na(played_as_black), "In progress", played_as_black)) %>%
      mutate(played_as_black = if_else(
        !played_as_black %in% c("In progress", "0", "1"),
        "0.5",
        played_as_black
      )) %>%
      mutate(played_as_white = as.numeric(played_as_white)) %>%
      mutate(played_as_black = as.numeric(played_as_black))

    # Drop the link to player stats API and member status columns.
    my_team_players <- my_team_players %>%
      select(c(
        username,
        played_as_white,
        played_as_black,
        board,
        time_out_count
      ))

  } else {
    my_team_players = empty_tibble
    warning(paste("Could not find team for", club_id, "for match", match_id))
  }
  return(my_team_players)
}

# Checks the data frame for the provided columns.If they do not exist, they're added and filled with NA values.
.add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if (length(add) != 0)
    df[add] <- NA
  return(df)
}

.getId <- function(url) {
  url_elements <- url %>% str_split_1('/')
  id <- url_elements %>% last()
  return(id)
}
