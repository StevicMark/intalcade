leaderboard_event <- function(input, output, session){
    observeEvent(input$leaderboardGameSelect,{
        renderLeaderBoardTable(input, output, session, input$leaderboardGameSelect)
    })
}
