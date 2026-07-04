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

  ## VARIABLES -----------------------------------------------------------------
  prepareSeterraVariables <- function(continent){
    countries <- sf::st_read("data/ne_50m_admin_0_countries.shp")
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
