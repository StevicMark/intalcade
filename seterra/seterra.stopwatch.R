stopwatch_functionality <- function(input, output, session){
## STOP WATCH FUNCTIONALITY --------------------------------------------------
  stopwatch <<- reactiveValues(
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
}