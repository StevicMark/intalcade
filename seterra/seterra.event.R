## MAP CLICK -----------------------------------------------------------------
seterra_event <- function(input, output, session){ 
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
      renderCountryPopup(input, output, session, click, clicked_country, fill_color)

      #check win condition
      if(nrow(countries_to_guess[!countries_to_guess$guessed,]) > 0){
        countries_left_to_guess <- countries_to_guess[!countries_to_guess$guessed,]
        current_country_to_guess <<- st_drop_geometry(countries_left_to_guess[sample(nrow(countries_left_to_guess), 1), ])
        renderCountryName(input, output, session)
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
        renderCountryPopup(input, output, session, click, clicked_country, "white")
      # country has not already been guessed
      }else{
        # add one try
        countries_to_guess[countries_to_guess$NE_ID == current_country_to_guess$NE_ID,]$tries <- countries_to_guess[countries_to_guess$NE_ID == current_country_to_guess$NE_ID,]$tries + 1
        # commit
        countries_to_guess <<- countries_to_guess
        # show name in pop up (red)
        renderCountryPopup(input, output, session, click, clicked_country, "red")
        if(countries_to_guess[countries_to_guess$NE_ID == current_country_to_guess$NE_ID,]$tries > max_tries){
          flashing$running <- T
        }
      }
    }
  })
}