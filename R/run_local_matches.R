## Runner to load local package R files and execute matches()
files <- list.files(path = "c:/Users/mhelke/Documents/Projects/ChessComClubManagement/R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(files, function(f) {
  tryCatch(source(f, local = FALSE), error = function(e) {
    message(sprintf("Error sourcing %s: %s", f, e$message))
  })
}))

cat("Sourced files:\n")
print(files)

cat("Running matches() for clubId=1-day-per-move-club\n")
res <- tryCatch(matches(clubId = '1-day-per-move-club'), error = function(e) { print(e); NULL })
print(res)
