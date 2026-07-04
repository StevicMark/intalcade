renderSeterraGame <- function(input, output, session, title){
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

  renderMap <- function(input, output, session){
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

  renderCountryName <- function(input, output, session){
    output$country_to_guess_ui <- renderUI({
      as.character(current_country_to_guess$NAME_EN)
    })
  }

  renderCountryPopup <- function(input, output, session, click, country, color){
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

  renderWon <- function(input, output, session){

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