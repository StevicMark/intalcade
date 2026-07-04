  # HOME SCREEN INTERACTION ----------------------------------------------------
homescreen_event <- function(input, output, session){
  observeEvent(input$wake_up, {
    renderHomescreen(input, output, session)
  })

  observeEvent(input$user_inactive, {
    renderIdleGif(input, output, session)
    # Tell JS to start the playlist
    session$sendCustomMessage("start_media", list())
  })

  observeEvent(input$homeScreen,{
    renderHomescreen(input, output, session)
  })

  observeEvent(input$submitGameWon,{
    if(input$playerGameWon != ""){
      new_score <- data.frame(rank = c(0), game = c(as.character(current_game)), score = c(current_score), time = c(current_time), player = c(as.character(input$playerGameWon)), stringsAsFactors = F)
      leaderboard <- bind_rows(leaderboard, new_score)
      leaderboard <<- leaderboard
      renderLeaderBoardPage(input, output, session, current_game)
      renderLeaderBoardTable(input, output, session, current_game)

      removeModal()
    }
  })

  observeEvent(input$cancelGameWon,{
    renderHomescreen(input, output, session)
    removeModal()
  })

  observeEvent(input$guessTheCountryAsia, {
    current_game <<- "Guess the country - Asia"
    prepareSeterraVariables("Asia")
    renderSeterraGame(input, output, session, "Guess the country - Asia")
    renderMap(input, output, session)
    renderCountryName(input, output, session)
  })

  observeEvent(input$guessTheCountryAfrica, {
    current_game <<- "Guess the country - Africa"
    prepareSeterraVariables("Africa")
    renderSeterraGame(input, output, session, "Guess the country - Africa")
    renderMap(input, output, session)
    renderCountryName(input, output, session)
  })

  observeEvent(input$guessTheCountrySA, {
    current_game <<- "Guess the country - South America"
    prepareSeterraVariables("South America")
    renderSeterraGame(input, output, session, "Guess the country - South America")
    renderMap(input, output, session)
    renderCountryName(input, output, session)
  })

  observeEvent(input$leaderboard, {
    renderLeaderBoardPage(input, output, session,)
  })
}