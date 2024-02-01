#' @import dplyr
#' @import cli
#' @importFrom httr GET
#' @importFrom stringr str_split_1
#' @importFrom lubridate as_datetime ymd
#' @importFrom jsonlite fromJSON

# Collects the details of the specified team match into a tibble
.getMatchDetails <- function(club_id, match_id, access_token) {
  empty_tibble <- tibble(
    username = character(),
    played_as_white = character(),
    played_as_black = character(),
    board = character(),
    time_out_count = character()
  )

  baseUrl <- "https://api.chess.com/pub/match/"
  endpoint <- paste0(baseUrl, match_id, sep = "", collapse = NULL)

  match_details_raw <- .fetch(endpoint, access_token)
  if (class(match_details_raw) != "list") {
    cli_warn("Match `{match_id}` cannot be found")
    return(NA)
  }

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

  # A result of 'lose' for both teams in a finished match indicates the match was canceled.
  is_canceled <-
    (
      match_details_raw$status == 'finished' &&
        team1$result == 'lose' && team2$result == 'lose'
    )

  if (found_matching_team) {
    # If the match was canceled return an empty tibble
    if (!is.na(is_canceled) && is_canceled) {
      cli_warn("Match `{match_id}` was canceled")
      return(empty_tibble)
    }

    # No players have registered yet, return an empty tibble
    if (!"players" %in% names(my_team)) {
      cli_warn("Can't find players registered for team `{club_id}` for match `{match_id}`")
      return(empty_tibble)
    }

    my_team_players <- my_team$players %>% as.data.frame()

    if (all(dim(my_team_players)) == 0) {
      cli_warn("No players registered for team `{club_id}` for match `{match_id}`")
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
      # Suppress warnings for 'NAs introduced by coercion'. NAs indicate in-progress games.
      mutate(played_as_white = suppressWarnings(as.numeric(played_as_white))) %>%
      mutate(played_as_black = suppressWarnings(as.numeric(played_as_black)))

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
    my_team_players <- empty_tibble
    cli_warn("Could not find team for `{club_id}` for match `{match_id}`")
  }
  return(my_team_players)
}

.getTournament <- function(endpoint, access_token = NA) {
  if (is.na(endpoint)) {
    return(NA)
  }
  cli_inform("Fetching tournament details")
  results <- .fetch(endpoint, access_token)
  if (class(results) != "list") {
    cli_warn("Failed to fetch tournament info")
    return(NA)
  }
  return(results$settings$allow_vacation)
}

# Checks the data frame for the provided columns.If they do not exist, they're added and filled with NA values.
.add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if (length(add) != 0) {
    df[add] <- NA
  }
  return(df)
}

.getId <- function(url) {
  url_elements <- url %>% str_split_1("/")
  id <- url_elements %>% last()
  return(id)
}

.fetch <- function(endpoint, token) {
  if (is.na(token)) {
    response <- GET(endpoint)
  } else {
    authorization <- paste0("Bearer ", token)
    response <-
      GET(endpoint, add_headers(Authorization = authorization))
  }

  status <- response$status_code
  data <- NA
  if (between(status, 200, 299)) {
    data <- fromJSON(rawToChar(response$content), flatten = TRUE)
  } else {
    cli_inform(c("Failed to fetch data", "x" = "HTTP {status} response received from {endpoint}"))
  }
  return(data)
}
