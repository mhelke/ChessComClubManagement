#' @import dplyr
#' @import cli
#' @importFrom httr GET
#' @importFrom stringr str_split_1
#' @importFrom lubridate as_datetime ymd
#' @importFrom purrr map
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr unnest

# Match Data ---------------------------

#' @name getMatchDetailsForMatches
#' @title Get details for club matches
#' @description Fetches match data for the given list of team matches for a particular club Id.
#' @param club_id ID of the club you want matches details for
#' @param match_ids List of IDs of the matches you want details for
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns Tibble of all specified match data
#' @source chess.com public API
#' @export
getMatchDetailsForMatches <-
  function(club_id, match_ids, access_token = NA) {
    match_details <- data.frame(
      username = character(),
      played_as_white = character(),
      played_as_black = character(),
      board = character(),
      time_out_count = character()
    )

    total_matches <- length(match_ids)
    cli_alert("Fetching {total_matches} matches")
    cli_progress_bar("Fetching matches...", total = total_matches)
    for (match in match_ids) {
      details <- .getMatchDetails(club_id, match, access_token)
      match_details <- match_details %>% rbind(details)
      cli_progress_update()
    }
    cli_progress_done()
    cli_alert_success("Finished fetching details for {total_matches} matches")
    return(match_details)
  }

#' @name getMatchIds
#' @title Get the Daily Match Ids for a given club
#' @description Fetches the daily match Ids for the given club and returns a list of Ids.
#' You can specify which match status to include (finished, in-progress, and upcoming) as well as the number of days in the past the match started.
#' A value of nDays = NA will return all matches that are available from the chess.com API.
#' @param club_id ID of the club you want match IDs for
#' @param include_finished Returns finished match IDs
#' @param include_in_progress Returns in-progress match IDs
#' @param include_upcoming Returns upcoming match IDs
#' @param nDays The number of days to look back for matches, based on the match start date
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns List of match IDs
#' @source chess.com public API
#' @export
getMatchIds <-
  function(club_id,
           include_finished = TRUE,
           include_in_progress = TRUE,
           include_upcoming = TRUE,
           nDays = NA,
           access_token = NA) {
    all_matches <-
      getMatchUrls(
        club_id,
        include_finished,
        include_in_progress,
        include_upcoming,
        nDays,
        access_token
      )

    total_matches <- length(all_matches)
    match_ids <-
      vector(mode = "character")

    for (match in all_matches) {
      url_elements <- match %>% str_split_1("/")
      match_id <- url_elements %>% last()
      match_ids <- append(match_ids, match_id)
    }
    cli_alert_success("Finished fetching {total_matches} matches")
    return(match_ids)
  }

#' @name getMatchUrls
#' @title Get the API URLs of matches for a given club
#' @description Fetches the URLs for the given club and returns a list of URLs.
#' You can specify which match status to include (finished, in-progress, and upcoming) as well as the number of days in the past the match started.
#' A value of nDays = NA will return all match URLs that are available from the chess.com API.
#' @param club_id ID of the club you want match URLs for
#' @param include_finished Returns finished match URLs
#' @param include_in_progress Returns in-progress match URLs
#' @param include_upcoming Returns upcoming match URLs
#' @param nDays The number of days to look back for matches, based on the match start date
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns List of match URLs
#' @seealso [getMatchIds()]
#' @source chess.com public API
#' @export
getMatchUrls <-
  function(club_id,
           include_finished = TRUE,
           include_in_progress = TRUE,
           include_upcoming = TRUE,
           nDays = NA,
           access_token = NA) {
    message <-
      ifelse(is.na(nDays), "all time", paste0("the past ", nDays, " days"))
    cli_inform("Fetching team match URLs from {message}")
    baseUrl <- "https://api.chess.com/pub/club/"
    endpoint <-
      paste0(baseUrl,
        club_id,
        "/matches",
        sep = "",
        collapse = NULL
      )

    club_matches_raw <- .fetch(endpoint, access_token)
    if (class(club_matches_raw) != "list") {
      cli_abort("Matches for `{club_id}` cannot be found")
    }

    matches <- vector(mode = "character")
    if (include_finished) {
      cli_inform("Including finished matches...")
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
      cli_inform("Including upcoming matches...")

      upcoming_matches <- club_matches_raw$registered %>%
        filter(time_class == "daily")
      upcoming_matches <- upcoming_matches$`@id` %>% as.list()

      matches <- append(matches, upcoming_matches)
    }
    if (include_in_progress) {
      cli_inform("Including in-progress matches...")

      in_progress_matches <- club_matches_raw$in_progress %>%
        filter(time_class == "daily")
      in_progress_matches <- in_progress_matches$`@id` %>% as.list()

      matches <- append(matches, in_progress_matches)
    }
    return(matches)
  }

# Leaderboard ---------------------------

#' @name getAllTimeLeaderBoard
#' @title Get All Time Leaderboard for Team Matches
#' @description Fetches the record of match results for all players who have ever played a match for the club and are currently members of the club.
#' Note: chess.com is temporarily limiting finished matches to the most recent 500 due to performance.
#' @param club_id ID of the club you want the leader board for
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns A Tibble of club members who have participated in matches and their all-time records
#' @note Chess.com public API only returns the most recent 500 completed matches due to performance issues. Until that is resolved by the chess.com team, this function may NOT return the all-time stats and results may not be accurate for all clubs.
#' @source chess.com public API
#' @export
getAllTimeLeaderBoard <- function(club_id, access_token = NA) {
  # Match IDs for all daily events
  match_ids <-
    getMatchIds(
      club_id,
      include_finished = TRUE,
      include_in_progress = TRUE,
      include_upcoming = FALSE,
      access_token = access_token
    )

  all_time_match_results <-
    getMatchDetailsForMatches(club_id, match_ids, access_token) %>%
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

# Match Results ---------------------------

#' @name getMatchResults
#' @title Get the results of team matches against other clubs
#' @description Fetches the completed team matches and returns a the given club's record against other clubs.
#' @param club_id ID of the club you want the results for
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns A Tibble of the club's team match record versus other clubs
#' @note Chess.com public API only returns the most recent 500 completed matches due to performance issues. Until that is resolved by the chess.com team, this function may NOT return all the clubs ever played.
#' @source chess.com public API
#' @export
getMatchResults <- function(club_id, access_token = NA) {
  baseUrl <- "https://api.chess.com/pub/club/"
  endpoint <-
    paste0(baseUrl,
      club_id,
      "/matches",
      sep = "",
      collapse = NULL
    )

  matches_raw <- .fetch(endpoint, access_token)
  if (class(matches_raw) != "list") {
    cli_abort("Matches for `{club_id}` cannot be found")
  }

  match_info <- matches_raw$finished %>%
    filter(time_class == "daily") %>%
    select(`@id`, -opponent, result)

  match_data_list <- list()
  total_matches <- length(match_info$`@id`)
  cli_alert("Fetching data for {total_matches} matches")
  cli_progress_bar("Fetching match data...", total = total_matches)
  for (match_id in match_info$`@id`) {
    match_data <- tryCatch(.fetch(match_id, access_token), error = function(e) NA)
    match_data_list <- append(match_data_list, list(match_data))
    cli_progress_update()
  }
  cli_progress_done()
  cli_alert_success("Finished fetching data for {total_matches} matches")

  match_info <- match_info %>%
    mutate(match_data = match_data_list) %>%
    filter(is.list(match_data) & !is.na(match_data))

  if (length(match_info$`@id`) == 0) {
    cli_warn("No matches found for club `{club_id}`")
    return(tibble())
  }

  match_info < match_info %>%
    select(-`@id`) %>%
    unnest_wider(match_data) %>%
    mutate(settings = map(settings, as.data.frame)) %>%
    unnest(settings) %>%
    rowwise() %>%
    mutate(
      opponent = case_when(
        .getId(teams$team1$`@id`) != club_id ~ teams$team1$name,
        .getId(teams$team2$`@id`) != club_id ~ teams$team2$name,
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      opponent_id = case_when(
        .getId(teams$team1$`@id`) != club_id ~ teams$team1$`@id`,
        .getId(teams$team2$`@id`) != club_id ~ teams$team2$`@id`,
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(team1_score = teams$team1$score) %>%
    mutate(team2_score = teams$team2$score) %>%
    ungroup() %>%
    select(
      opponent,
      result,
      rules,
      time_control,
      opponent_id,
      team1_score,
      team2_score
    )

  matches <- match_info %>%
    mutate(canceled = if_else(result == "draw" & team1_score == 0 & team2_score == 0, TRUE, FALSE)) %>%
    filter(!canceled) %>%
    select(-canceled, -team1_score, -team2_score) %>%
    mutate(wins = if_else(result == "win", 1, 0)) %>%
    mutate(draws = if_else(result == "draw", 1, 0)) %>%
    mutate(losses = if_else(result == "lose", 1, 0)) %>%
    rowwise() %>%
    mutate(time_control = as.numeric(str_split_1(time_control, "/")[2]) / 86400) %>%
    ungroup() %>%
    mutate(rules = ifelse(rules == "chess", "Standard", rules)) %>%
    mutate(rules = ifelse(rules == "chess960", "Chess960", rules)) %>%
    group_by(opponent, rules, time_control, opponent_id) %>%
    summarise(
      matches_played = n(),
      wins = sum(wins),
      draws = sum(draws),
      losses = sum(losses),
    ) %>%
    arrange(desc(matches_played)) %>%
    rowwise() %>%
    mutate(
      opponent_club_members = {
      club_details <- .fetch(opponent_id, access_token)
      if (!is.list(club_details)) {
        NA_integer_
      } else {
        club_details$members_count
      }
      }
    ) %>%
    ungroup() %>% 
    select(
      opponent,
      rules,
      time_control,
      opponent_club_members,
      matches_played,
      wins,
      draws,
      losses
    )

  return(matches)
}

# Match Removals ---------------------------

#' @name getPlayersToRemoveFromMatch
#' @title Get a list of users ineligible to participate in a given match
#' @description Retrieves players registered for a match that are ineligible to participate
#' @param match_id ID of the match to manage
#' @param club_id ID of the club you manage
#' @param max_timeouts The maximum timeout percentage allowed for participation
#' @param min_total_games The minimum number of completed games allowed for participation
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns A list of players ineligible to participate in the match, filtered by the provided criteria
#' @source chess.com public API
#' @export
getPlayersToRemoveFromMatch <-
  function(match_id,
           club_id,
           max_timeouts,
           min_total_games,
           access_token = NA) {
    if (is.na(match_id)) {
      cli_abort("{.var match_id} cannot be NA")
    }
    if (is.na(club_id)) {
      cli_abort("{.var club_id} cannot be NA")
    }
    if (is.na(max_timeouts)) {
      cli_abort("{.var max_timeouts} cannot be NA")
    }
    if (is.na(min_total_games)) {
      cli_abort("{.var min_total_games} cannot be NA")
    }

    match_details <-
      getMatchDetailsForMatches(club_id, match_id, access_token)

    players <- match_details$username

    user_details <- data.frame(
      username = character(),
      url = character(),
      joined_site = numeric(),
      last_online = numeric(),
      country = character(),
      daily_rating = numeric(),
      daily_960_rating = numeric(),
      time_per_move = numeric(),
      timeout_percent = numeric(),
      activity = character(),
      total_games = numeric()
    )

    total_players <- length(players)

    if (total_players == 0) {
      cli_abort("No players are signed up for match `{match_id}` on team `{club_id}`")
    }

    cli_alert("Fetching stats for {total_players} users")
    cli_progress_bar("Fetching stats...", total = total_players)
    for (player in players) {
      stats <- getUserStats(user_id = player, access_token)
      if (class(stats) == "data.frame") {
        user_details <- user_details %>% rbind(stats)
      }
      cli_progress_update()
    }
    cli_progress_done()
    cli_alert_success("Finished fetching details for {total_players} users")

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
