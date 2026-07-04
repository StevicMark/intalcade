csv_file <- "data/leaderboard.csv"

server <- function(input, output, session) {

  if (file.exists(csv_file)) {
    leaderboard <<- read.csv(csv_file, stringsAsFactors = FALSE, colClasses = c("rank" = "integer", "game" = "character", "score" = "integer", "time" = "integer", "player" = "character"))
  } else {
    leaderboard <<- data.frame(rank = as.integer(), game = as.character(), score = as.integer(), time = as.integer(), player = as.character(), stringsAsFactors = F)
  }
  games <<- c("Guess the country - Asia", "Guess the country - South America", "Guess the country - Africa")
  current_game <<- NULL
  current_score <<- NULL
  current_time <<- NULL
  max_tries <<- 3
  screen_state <<- "home"

  # RENDER FUNCTIONS -----------------------------------------------------------
  lapply(list.files("homescreen", pattern = "\\.R$", full.names = TRUE), source)
  lapply(list.files("leaderboard", pattern = "\\.R$", full.names = TRUE), source)
  lapply(list.files("seterra", pattern = "\\.R$", full.names = TRUE), source)


  renderHomescreen(input, output, session)
  homescreen_event(input, output, session)
  leaderboard_event(input, output, session)
  seterra_event(input, output, session)
  stopwatch_functionality(input, output, session)
  
  session$onSessionEnded(function() {
    write.csv(leaderboard, csv_file, row.names = FALSE)
  })

}
