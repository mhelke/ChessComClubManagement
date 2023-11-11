#' @import dplyr
#' @import cli
#' @importFrom httr GET
#' @importFrom stringr str_split_1
#' @importFrom lubridate as_datetime ymd

#' @name getAllGamesForPlayer
#' @title Get the stats on the timeouts for a player
#' @description Fetches a player's daily games from the archive
#' @param user_id ID of the user you want stats for
#' @param year Which year to start querying the user's archive from
#' @param month Which month to start querying the user's archive from
#' @param nmonths The number of months to query the archives
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns All daily games for a player
#' @source chess.com public API
#' @seealso [getGameResultsForPlayer()] `getGameResultsForPlayer`
#' @export
getAllGamesForPlayer <-
  function(user_id,
           year,
           month,
           nmonths,
           access_token = NA) {
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

    total_months <- length(JSON_url)
    cli_alert_info("Fetching games for `{user_id}`")
    cli_alert("Fetching games from past {total_months} months")
    cli_progress_bar("Fetching games...", total = total_months)

    for (url in JSON_url) {
      player_games_raw <- .fetch(url, access_token)
      if (class(player_games_raw) != "list") {
        cli_warn("Games cannot be found for user {user_id} for {month}/{year}.")
        next
      }

      player_games <- player_games_raw$games %>% as_tibble()

      # The match and tournament columns are only included when not NA
      player_games <- .add_cols(player_games, cols)
      player_games <- player_games %>%
        select(all_of(cols))

      all_player_games <- all_player_games %>%
        rbind(player_games)

      cli_progress_update()
    }
    cli_progress_done()
    cli_alert_success("Done fetching games for {user_id}")

    # Filter by daily games and opponent stats
    player_stats <- all_player_games %>%
      filter(time_class == "daily") %>%
      mutate(color = if_else(tolower(white.username) == tolower(user_id), "w", "b")) %>%
      mutate(username = if_else(color == "w", white.username, black.username)) %>%
      mutate(result = if_else(color == "w", white.result, black.result)) %>%
      select(-white.username, -black.username, -white.result, -black.result) %>%
      # Daily games are given in the format for 1/<seconds>
      mutate(time_control = substring(time_control, 3)) %>%
      # Convert seconds to days
      mutate(time_control = as.numeric(time_control) / (60 * 60 * 24))

    return(player_stats)
  }

#' @name getGameResultsForPlayer
#' @title Get the results and the timeouts of daily games for a player
#' @description Query a player's game archive to find all match and tournament games
#' @param user_id ID of the user you want stats for
#' @param year Which year to start querying the user's archive from
#' @param month Which month to start querying the user's archive from
#' @param nmonths The number of months to query the archives
#' @param include_vacation Boolean for whether to check the player's tournament games for vacation rules
#' @param access_token The access token for chess.com APIs obtained through authorization
#' @returns Tibble of the number of games for a user for each event type/time control/result grouping
#' @source chess.com public API
#' @export
getGameResultsForPlayer <-
  function(user_id,
           year,
           month,
           nmonths,
           include_vacation = FALSE,
           access_token = NA) {
    player_stats <-
      getAllGamesForPlayer(user_id, year, month, nmonths, access_token)

    # Filter for match/tournament timeouts
    results <- player_stats %>%
      select(username, result, time_control, match, tournament) %>%
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
      filter(event == "match" | event == "tournament")



    # Check the tournaments endpoint
    if (include_vacation & nrow(results) > 0) {
      results <- results %>%
        mutate(vacation = sapply(tournament, .getTournament, access_token = access_token)) %>%
        mutate(vacation = if_else(event == "match", TRUE, vacation))
    } else {
      results <- results %>%
        mutate(vacation = NA)
    }

    results <- results %>%
      group_by(time_control, event, result, vacation) %>%
      summarise(
        username = user_id,
        total_games = n()
      )

    return(results)
  }
