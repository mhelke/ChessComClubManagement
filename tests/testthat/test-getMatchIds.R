test_that("test match ids", {
  mock_fetch <- function(endpoint) {

    current_time <- as.numeric(as.POSIXct(Sys.time()))
    start <- current_time - 86400

    finished <- data.frame(
      name = character(),
      `@id` = character(),
      opponent = character(),
      start_time = numeric(),
      time_class = character(),
      result = character()
    )
    finished <- finished %>% rename(`@id` = "X.id")
    finished[1,] = c("finished match", "finished-url/1", "opponent-1", start, "daily", "win")
    finished[2,] = c("finished match 2", "finished-url/2", "opponent-1", start - (86400 *3), "daily", "win")
    finished[3,] = c("not-included", "not-included-url/3", "opponent-1", start, "blitz", "win")

    in_progress <- data.frame(
      name = character(),
      `@id` = character(),
      opponent = character(),
      start_time = numeric(),
      time_class = character()
    )
    in_progress <- in_progress %>% rename(`@id` = "X.id")
    in_progress[1,] = c("in_progress match", "in_progress-url/4", "opponent-1", start, "daily")
    in_progress[2,] = c("not-included", "not-included-url/5", "opponent-1", start, "blitz")

    upcoming <- data.frame(
      name = character(),
      `@id` = character(),
      opponent = character(),
      start_time = numeric(),
      time_class = character()
    )

    upcoming <- upcoming %>% rename(`@id` = "X.id")

    upcoming[1,] = c("upcoming match", "upcoming-url/6", "opponent-1", start, "daily")
    upcoming[2,] = c("not-included", "not-included-url/7", "opponent-1", start, "blitz")

    result <- list(finished = finished, in_progress = in_progress, registered = upcoming)
    result$finished

    return(result)
  }

  mockr::local_mock(.fetch =  mock_fetch)
  ids <- getMatchIds(
    "test-club",
    TRUE,
    TRUE,
    TRUE,
    nDays = NA
  )
  expect_equal(length(ids), 4)

  ids <- getMatchIds(
    "test-club",
    TRUE,
    FALSE,
    FALSE,
    nDays = NA
  )
  expect_equal(length(ids), 2)
  expect_equal(ids[1], "1")
  expect_equal(ids[2], "2")

  ids <- getMatchIds(
    "test-club",
    FALSE,
    TRUE,
    FALSE,
    nDays = NA
  )
  expect_equal(length(ids), 1)
  expect_equal(ids[1], "4")

  ids <- getMatchIds(
    "test-club",
    FALSE,
    FALSE,
    TRUE,
    nDays = NA
  )
  expect_equal(length(ids), 1)
  expect_equal(ids[1], "6")

  ids <- getMatchIds(
    "test-club",
    TRUE,
    TRUE,
    TRUE,
    nDays = 2
  )

  expect_equal(length(ids), 3)
  expect_equal(unlist(ids)[1], "1")
  expect_equal(unlist(ids)[2], "6")
  expect_equal(unlist(ids)[3], "4")
})
