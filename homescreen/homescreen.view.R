renderHomescreen <- function(input, output, session){
    current_game <<- NULL
    current_score <<- NULL
    current_time <<- NULL

    screen_state <<- "home"
    session$sendCustomMessage("screen_state", list(state = "home"))

    output$app_page <- renderUI({
      div(style = "width:100%;height:100%;display:flex;justify-content:center;flex-direction: column;position:absolute;left:0;background-color:#4B5AA7;",
      div(style="width: 100%;height: 80%;display:flex;flex-direction: column;align-items:center;justify-content:space-between;", class= "homescreen-font",
          div(class = "homescreen-messagebox",
              p("Let’s be real: it’s game over for imperialism. And every strong movement for international solidarity needs solid geographical knowledge. So come test yours, learn, laugh, and maybe snag a prize along the way.")
          ),
          div(style="display:flex;flex-direction: column;width:80%",
              titlePanel("Pick a game"),
              div(style="display:flex;flex-direction: column;gap:10px;",
                shiny::actionButton("guessTheCountryAsia", "Guess the country - Asia", icon = icon("map"), class = "homescreen-button"),
                shiny::actionButton("guessTheCountrySA", "Guess the country - South America", icon = icon("map"), class = "homescreen-button"),
                shiny::actionButton("guessTheCountryAfrica", "Guess the country - Africa", icon = icon("map"), class = "homescreen-button")
              )

          ),
          div(style="display:flex;flex-direction: column;width:80%;",
              titlePanel("Look at the scores"),
              shiny::actionButton("leaderboard", "Open Leaderboard", icon = icon("trophy"), class = "homescreen-button")
          ),
          div(class = "homescreen-messagebox",
            p("⚠️ Note: We don’t necessarily condone the nation-state or nation-form. Borders are often colonial constructs. We do not endorse any border claims on this map. It's just the only base file available.")
          )
      )
      )
    })
  }

renderIdleGif=function(input, output, session){
    output$app_page <- renderUI({
      div(style = "position:absolute;left: 0;width:100%;height:100%;display:flex;flex-direction:column;align-items:center;",
          tags$img(src = "idle.gif", height = "100%", width = "100%")
      )
  })
  }