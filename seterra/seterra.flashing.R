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