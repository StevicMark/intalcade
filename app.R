#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(jsonlite)


csv_file <- "data/leaderboard.csv"
playlist <- c("song1.m4a", "song2.m4a", "song3.m4a")

# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "intalcade.css")),

  tags$head(
    tags$script(HTML(sprintf("
      let inactivityTimeout;
      const inactivityLimit = 60000; // 1 minute
      const playlist = %s;
      let currentIndex = 0;
      let audioPlayer;
      let onHome = true;
      let idleMode = false;  // tracks whether we're in the idle / media mode

      function resetInactivityTimer() {
        clearTimeout(inactivityTimeout);
        // If we are in idle mode, ignore reset?
        inactivityTimeout = setTimeout(function() {
          if (onHome && !idleMode) {
            Shiny.setInputValue('user_inactive', new Date().getTime());
          }
        }, inactivityLimit);
      }

      function playNext() {
        if (currentIndex >= playlist.length) return;
        const audioSrc = playlist[currentIndex];
        if (audioPlayer) {
          audioPlayer.remove();
        }
        audioPlayer = document.createElement('audio');
        audioPlayer.src = audioSrc;
        audioPlayer.autoplay = true;
        audioPlayer.type = 'audio/mp4';
        audioPlayer.addEventListener('ended', function() {
          currentIndex++;
          playNext();
        });
        document.body.appendChild(audioPlayer);
      }

      function startPlaylist() {
        if (!playlist.length) return;
        idleMode = true;
        currentIndex = 0;
        playNext();
      }

      function wakeUp() {
        if (idleMode) {
          idleMode = false;
          // tell Shiny to go home
          Shiny.setInputValue('wake_up', new Date().getTime());
          // stop any audio
          if (audioPlayer) {
            audioPlayer.pause();
            audioPlayer.remove();
            audioPlayer = null;
          }
          // also remove GIF etc in Shiny side
        }
      }

      // mousemove and keydown also trigger wake up if idle
      document.onmousemove = function(e) {
        resetInactivityTimer();
        wakeUp();
      };
      document.onkeydown = function(e) {
        resetInactivityTimer();
        wakeUp();
      };
      document.onload = resetInactivityTimer;

      Shiny.addCustomMessageHandler('screen_state', function(message) {
        onHome = (message.state === 'home');
        if (!onHome) {
          // leaving home => stop audio, exit idle
          idleMode = false;
          if (audioPlayer) {
            audioPlayer.pause();
            audioPlayer.remove();
            audioPlayer = null;
          }
        }
      });

      Shiny.addCustomMessageHandler('start_media', function(message) {
        if (onHome && !idleMode) {
          startPlaylist();
        }
      });
    ", jsonlite::toJSON(playlist))))
  ),

  uiOutput("app_page"),
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  if (file.exists(csv_file)) {
    leaderboard <<- read.csv(csv_file, stringsAsFactors = FALSE, colClasses = c("rank" = "integer", "game" = "character", "score" = "integer", "time" = "integer", "player" = "character"))
  } else {
    leaderboard <<- data.frame(rank = as.integer(), game = as.character(), score = as.integer(), time = as.integer(), player = as.character(), stringsAsFactors = F)
  }
  games <- c("Guess the country - Asia", "Guess the country - South America", "Guess the country - Africa")
  current_game <<- NULL
  current_score <<- NULL
  current_time <<- NULL
  max_tries <<- 3
  screen_state <<- "home"

  # RENDER FUNCTIONS -----------------------------------------------------------

  renderHomescreen <- function(){
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

  renderLeaderBoardPage <- function(game = NULL){
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

  renderLeaderBoardTable <- function(game){

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

  renderSeterraGame <- function(title){
    stopwatch$elapsed <- 0
    stopwatch$running <- T

    screen_state <<- "game"
    session$sendCustomMessage("screen_state", list(state = "game"))

    output$app_page <- renderUI({
      div(style="width: 100%;height: 100%;display:flex;flex-direction: column;position: absolute;left:0",
          div(style="display:flex;justify-content: center;",
              actionButton(inputId = "homeScreen", label = NULL, icon = icon("arrow-left"), class = "back-arrow"),
              div(class = "country-to-guess guess-font",
                  uiOutput("country_to_guess_ui")
                  ),
              div(class = "timer guess-font",
                 textOutput("elapsed_time")
              )


          ),
          div(style="display:flex;height:100%",
              leafletOutput("map", height = "100%")
          )
      )
    })
  }

  renderMap <- function(){
    bbox <- st_bbox(countries_to_guess)
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(
        maxBounds = list(
          c(as.numeric(bbox["ymin"]), as.numeric(bbox["xmin"])),  # SW corner (lat, lng)
          c(as.numeric(bbox["ymax"]), as.numeric(bbox["xmax"]))   # NE corner (lat, lng)
        ),
        maxBoundsViscosity = 1          # Prevents dragging outside bounds
      )) %>%
        addPolygons(data = countries_to_guess, fillColor = "darkgreen", weight = 1, layerId = ~NE_ID,
                    color = "black", fillOpacity = 0.6) %>%
        # addPolygons(data = other_countries, fillColor = "grey", weight = 1, layerId = ~NE_ID,
        #             color = "black", fillOpacity = 0.6) %>%
        fitBounds(
          lng1 = as.numeric(bbox["xmin"]),
          lat1 = as.numeric(bbox["ymin"]),
          lng2 = as.numeric(bbox["xmax"]),
          lat2 = as.numeric(bbox["ymax"])
        ) %>%
        htmlwidgets::onRender(paste0("
      function(el, x) {
        var map = this;
        var bounds = L.latLngBounds(
          L.latLng(",as.numeric(bbox["ymin"]),", ",as.numeric(bbox["xmin"]),"),
          L.latLng(",as.numeric(bbox["ymax"]),", ",as.numeric(bbox["xmax"]),")
        );
        var minZoom = map.getBoundsZoom(bounds, false);
        map.setMinZoom(minZoom);
        var zoomControl = this.zoomControl;
        this.removeControl(zoomControl);
      }
    "))
    })
  }

  renderCountryName <- function(){
    output$country_to_guess_ui <- renderUI({
      as.character(current_country_to_guess$NAME_EN)
    })
  }

  renderWon <- function(){

    modal_tmp<-modalDialog(
      fluidPage(
        div(style="display:flex;flex-direction:column;align-items:center;width:100%;padding: 10px 20px;", class = "homescreen-messagebox homescreen-font",
            titlePanel("Congrats!"),
            p(paste0("Score: ",current_score, "%")),
            # p(paste0("Your time is: ",time)), #check if it is in the top 10
            textInput(inputId = "playerGameWon", label = NULL, placeholder = "Enter your name ..."),
            div(style="display:flex;flex-direction: row;justify-content: center; gap:5px;",
                actionButton(inputId = "cancelGameWon", label = "Cancel"),
                actionButton(inputId = "submitGameWon", label = "Submit"),
                )
        )
        )
      ,size="s"
      ,footer = NULL
    )
    showModal(modal_tmp)
  }

  renderIdleGif=function(){
    output$app_page <- renderUI({
      div(style = "position:absolute;left: 0;width:100%;height:100%;display:flex;flex-direction:column;align-items:center;",
          tags$img(src = "idle.gif", height = "100%", width = "100%")
      )
  })
  }


  renderHomescreen()

  # HOME SCREEN INTERACTION ----------------------------------------------------

  observeEvent(input$wake_up, {
    renderHomescreen()
  })

  observeEvent(input$user_inactive, {
    renderIdleGif()
    # Tell JS to start the playlist
    session$sendCustomMessage("start_media", list())
  })

  observeEvent(input$homeScreen,{
    renderHomescreen()
  })

  observeEvent(input$submitGameWon,{
    if(input$playerGameWon != ""){
      new_score <- data.frame(rank = c(0), game = c(as.character(current_game)), score = c(current_score), time = c(current_time), player = c(as.character(input$playerGameWon)), stringsAsFactors = F)
      leaderboard <- bind_rows(leaderboard, new_score)
      leaderboard <<- leaderboard
      renderLeaderBoardPage(current_game)
      renderLeaderBoardTable(current_game)

      removeModal()
    }
  })

  observeEvent(input$cancelGameWon,{
    renderHomescreen()
    removeModal()
  })

  observeEvent(input$guessTheCountryAsia, {
    current_game <<- "Guess the country - Asia"
    prepareSeterraVariables("Asia")
    renderSeterraGame("Guess the country - Asia")
    renderMap()
    renderCountryName()
  })

  observeEvent(input$guessTheCountryAfrica, {
    current_game <<- "Guess the country - Africa"
    prepareSeterraVariables("Africa")
    renderSeterraGame("Guess the country - Africa")
    renderMap()
    renderCountryName()
  })

  observeEvent(input$guessTheCountrySA, {
    current_game <<- "Guess the country - South America"
    prepareSeterraVariables("South America")
    renderSeterraGame("Guess the country - South America")
    renderMap()
    renderCountryName()
  })

  observeEvent(input$leaderboard, {
    renderLeaderBoardPage()
    # renderLeaderBoardTable("Guess the country - South America")
  })

  observeEvent(input$leaderboardGameSelect,{
    renderLeaderBoardTable(input$leaderboardGameSelect)
  })


  # GUESS THE COUNTRY ----------------------------------------------------------
  ## HELPER FUNCTIONS ----------------------------------------------------------
  getColorFromTries <- function(tries){
    color <- "darkgreen"
    if(tries == 1){
      color <- "white"
    }else if(tries == 2){
      color <- "gold"
    }else if(tries == 3){
      color <- "orange"
    }else if(tries > 3){
      color <- "red"
    }
  }

  showCountryPopup <- function(click, country, color){
    clicked_country_row <- countries_to_guess[countries_to_guess$NE_ID == country,]
    popup_content <- sprintf(
      "<span class = 'country-name-popup' style='background-color:%s;'>%s</span>",
      color,
      clicked_country_row$NAME_EN
    )

    leafletProxy("map") %>%
      addPopups(
        lng = click$lng,
        lat = click$lat,
        popup = popup_content,
        layerId = "dynamic_popup",
        options = popupOptions(closeButton = FALSE)
      )
    later::later(function() {
      leafletProxy("map", session = session) %>% removePopup("dynamic_popup")
    }, delay = 1)
  }

  ## VARIABLES -----------------------------------------------------------------
  prepareSeterraVariables <- function(continent){
    countries <- st_read("data/ne_50m_admin_0_countries.shp")
    # countries <- countries[countries$TYPE == "Country",]

    countries_to_guess <- countries[countries$CONTINENT == continent,]
    countries_to_guess <- countries_to_guess[countries_to_guess$NAME_EN != "Somaliland",]
    other_countries <- countries[countries$CONTINENT != continent,]


    countries_to_guess <- countries_to_guess[,c("NAME_EN", "NE_ID")]
    countries_to_guess$guessed <- FALSE
    countries_to_guess$tries <- 0

    # countries_to_guess <- head(countries_to_guess, 4)

    countries_to_guess <<- countries_to_guess
    current_country_to_guess <<- st_drop_geometry(countries_to_guess[sample(nrow(countries_to_guess), 1), ])

  }

  ## STOP WATCH FUNCTIONALITY --------------------------------------------------
  stopwatch <- reactiveValues(
    running = T,
    elapsed = 0
  )

  # Timer to update elapsed time
  observe({
    req(stopwatch$running)
    invalidateLater(1000)  # update every 100 ms
    isolate({
      stopwatch$elapsed <- stopwatch$elapsed + 1
    })
  })

  # Format time as mm:ss
  formatted_time <- reactive({
    minutes <- floor(stopwatch$elapsed / 60)
    seconds <- stopwatch$elapsed %% 60
    sprintf("%02d:%02d", minutes, seconds)
  })

  # Display elapsed time
  output$elapsed_time <- renderText({
    paste(formatted_time())
  })

  ## FLASHING FUNCTIONALITY ----------------------------------------------------
  flashing <- reactiveValues(
    running = FALSE,
    interval = 500
  )
  flashingColor <<- "red"
  observe({
    req(flashing$running)
    invalidateLater(flashing$interval)

    # Alternate color
    if (flashingColor == "red") {
      flashingColor <<- "white"
    } else {
      flashingColor <<- "red"
    }

    # Update map
    leafletProxy("map") %>%
      removeShape(layerId = current_country_to_guess$NE_ID) %>%
      addPolygons(
        data = countries_to_guess[countries_to_guess$NE_ID == current_country_to_guess$NE_ID, ],
        layerId = current_country_to_guess$NE_ID,
        fillColor = flashingColor,
        color = "black",
        weight = 1,
        fillOpacity = 0.9
      )
  })


  ## MAP CLICK -----------------------------------------------------------------
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    # click$id will return the layerId that was clicked
    clicked_country <- click$id

    req(isTruthy(current_country_to_guess))
    # clicked the correct country
    if(clicked_country == current_country_to_guess$NE_ID){
      # add one try
      countries_to_guess[countries_to_guess$NE_ID == clicked_country,]$tries <- countries_to_guess[countries_to_guess$NE_ID == clicked_country,]$tries + 1
      # mark as guessed
      countries_to_guess[countries_to_guess$NE_ID == clicked_country,]$guessed <- TRUE
      # commit
      countries_to_guess <<- countries_to_guess
      # stop flashing if it is on
      flashing$running <- F
      # calculate color
      fill_color <- getColorFromTries(countries_to_guess[countries_to_guess$NE_ID == clicked_country,]$tries)
      # color polygon
      leafletProxy("map") %>%
        removeShape(layerId = clicked_country) %>%
        addPolygons(
          data = countries_to_guess[countries_to_guess$NE_ID == clicked_country, ],
          layerId = clicked_country,
          fillColor = fill_color,
          color = "black",
          weight = 1,
          fillOpacity = 0.9
        )
      # show name in pop up
      showCountryPopup(click, clicked_country, fill_color)

      #check win condition
      if(nrow(countries_to_guess[!countries_to_guess$guessed,]) > 0){
        countries_left_to_guess <- countries_to_guess[!countries_to_guess$guessed,]
        current_country_to_guess <<- st_drop_geometry(countries_left_to_guess[sample(nrow(countries_left_to_guess), 1), ])
        renderCountryName()
      }else{
        current_country_to_guess <<- NULL
        stopwatch$running <- F
        current_time <<- stopwatch$elapsed
        score <- 100 * (1 - sum(countries_to_guess$tries - 1) / (length(countries_to_guess$tries) * (max_tries - 1)))
        score <- round(score)
        current_score <<- score
        renderWon()
      }
    # clicked the wrong country
    }else{
      # country has already been guessed
      if(countries_to_guess[countries_to_guess$NE_ID == clicked_country,]$guessed){
        # show name in pop up (white)
        showCountryPopup(click, clicked_country, "white")
      # country has not already been guessed
      }else{
        # add one try
        countries_to_guess[countries_to_guess$NE_ID == current_country_to_guess$NE_ID,]$tries <- countries_to_guess[countries_to_guess$NE_ID == current_country_to_guess$NE_ID,]$tries + 1
        # commit
        countries_to_guess <<- countries_to_guess
        # show name in pop up (red)
        showCountryPopup(click, clicked_country, "red")
        if(countries_to_guess[countries_to_guess$NE_ID == current_country_to_guess$NE_ID,]$tries > max_tries){
          flashing$running <- T
        }
      }
    }
  })

  session$onSessionEnded(function() {
    write.csv(leaderboard, csv_file, row.names = FALSE)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
