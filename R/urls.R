
wikipedia_poll_url <- function(election_year) {
  glue(
    "https://en.wikipedia.org/wiki/Opinion_polling_for_the_{election_year}_New_Zealand_general_election"
  )
}

election_results_summary <- function(election_year) {
  glue(
    "data/results/{election_year}.csv"
  )
}
