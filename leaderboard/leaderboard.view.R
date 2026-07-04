 renderLeaderBoardPage <- function(input, output, session, game = NULL){
    leaderboard <- leaderboard %>%
      group_by(game) %>%
      arrange(desc(score), time, .by_group = TRUE) %>%
      mutate(rank = row_number())
    leaderboard <<- leaderboard

    screen_state <<- "home"
    session$sendCustomMessage("screen_state", list(state = "home"))

    output$app_page <- renderUI({
      div(style="width:100%;height:100%;display:flex;justify-content:center;flex-direction: column;position:absolute;left:0;background-color:#4B5AA7;",
          div(style="width: 100%;height: 80%;display:flex;flex-direction: column;align-items:center;justify-content:space-between;", class= "homescreen-font",
              div(class = "homescreen-messagebox",
                  p("🏆 Highest % correct wins! Ties go to the fastest guessers.
                  Prizes: Sat: 6:00 PM @ INTAL tent / Sun: 4:30 PM @ INTAL tent")
              ),
              titlePanel("Leaderboard"),
              div(style = "", selectInput(inputId = "leaderboardGameSelect", label = NULL, choices = games, selected = game, selectize = F, width = "450px")),
              div(style = "width:80%;height:100%;",
                DT::DTOutput("leaderboardDT")
              ),
            actionButton(inputId = "homeScreen", label = "Return to main menu and try to beat the high score !", icon = icon("home"), class = "homescreen-button")
      ))
    })
  }

  renderLeaderBoardTable <- function(input, output, session, game){

    output$leaderboardDT <- DT::renderDT({
      DT::datatable(
        leaderboard[leaderboard$game == game, c("rank", "player", "score", "time")],
        colnames = c("Rank", "Player", "Score", "Time"),
        rownames = FALSE,
        selection = "none",
        options = list(
          paging = T,
          searching = T,
          searchHighlight = T,
          pageLength = 5,
          ordering = FALSE,
          dom = 'ftp',
          language = list(emptyTable = "No highscores yet"),
          columnDefs = list(
            list(
              targets = 2,
              render = JS("function(data, type, row, meta) { return data + '%'; }")
            ),
            list(
              targets = 3,
              render = JS(  "function(data, type, row, meta) {",
                            "  var minutes = Math.floor(data / 60);",
                            "  var seconds = data % 60;",
                            "  return ('0' + minutes).slice(-2) + ':' + ('0' + seconds).slice(-2); }")
            )
          )
        )
      )},server = T)
  }