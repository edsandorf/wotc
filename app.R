#' PICO: Outreach
#'
#' This is a Shiny application written for the PICO project outreach workshop
#' in Tromsø.
#'
#' Author: Erlend Dancke Sandorf
#'
#'

# Load packages ----
library(tidyverse)
library(shiny)
library(shinyjs)
library(pool)
library(DBI)
library(RMariaDB)
library(config)
library(gt)

# Source the global file
source("global.R")

# Get the connection details
db_config <- config::get("dataconnection")

# Set up the pool for effective handling of multiple connections
pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = db_config$dbname,
  host = db_config$host,
  username = db_config$username,
  password = db_config$password
)


# User interface ----
ui <- fluidPage(
  class = "page",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  shinyjs::useShinyjs(),
  # The main window
  fluidRow(
    class = "ui-row",
    shiny::numericInput("guess",
                        label = "Hvor mange baller er det i akvariumet?",
                        value= "",
                        width = "100%"),
  
    actionButton("submit", "Send inn ditt svar")  
  ),
  fluidRow(
    class = "display-calculations",
    shinyjs::hidden(
      gt_output(outputId = "calculations")
    )
  ),
  fluidRow(
    class = "reset-button-row",
    actionButton("reset", "Prøv igjen")
  )
)

# Server side ----
server <- function(input, output, session) {
  # Useful variables
  db_table <- "ball_guesses"
  true_value <- 300
  
  # Create reactive values
  individual_error <- reactiveVal(0)
  avg_individual_error <- reactiveVal(0)
  group_guess <- reactiveVal(0)
  group_error <- reactiveVal(0)
  wisdom <- reactiveVal(0)
    
  # Create the tibble of guesses
  guesses <- tibble(
    id = paste0(sample(c(letters, LETTERS, 0:9), 10), collapse = "") ,
    timestamp_start = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    guess = NA
  )

  # Read in the other guesses from the database
  db_guesses <- dbReadTable(pool, db_table)
  
  # Reset the application ----
  observeEvent(input$reset, {
    shinyjs::refresh() # Does this trigger onSessionEnded()?
  })
  
  # When the session ends ----
  session$onSessionEnded(
    function() {

    }
  )
  
  # Submit guess ----
  observeEvent(input$submit, {
    # Get user guess (NB! Adding individual guess to all other guesses)
    individual_guess <- input$guess
    all_guesses <- c(pull(db_guesses, guess), individual_guess)
    
    # Submit the guess to the database
    guesses$guess <<- individual_guess
    
    dbWriteTable(
      conn = pool,
      name = db_table,
      value = guesses,
      append = TRUE
    )
    
    # Calculate 
    individual_error(calc_individual_error(individual_guess, true_value))
    avg_individual_error(calc_avg_individual_error(all_guesses, true_value))
    group_guess(calc_group_guess(all_guesses))
    group_error(calc_group_error(group_guess(), true_value))
    wisdom(calc_wisdom(avg_individual_error(), group_error()))
    
    # Toggle states
    shinyjs::show("calculations")
    shinyjs::disable("submit")
  })
  
  # Create a nice looking output table using GT
  output$calculations <- render_gt(
    tibble(
      ind_epsilon = individual_error(),
      ind_epsilon_avg = avg_individual_error(),
      group_est = group_guess(),
      group_epsilon = group_error(),
      wisdom_number = wisdom()
    ) %>% 
      gt() %>% 
      cols_label(
        ind_epsilon = "Ditt avvik",
        ind_epsilon_avg = "Gjennomsnittlig avvik",
        group_est = "Gruppas gjetning",
        group_epsilon = "Gruppas avvik",
        wisdom_number = "Visdommens nummer"
      ) %>% 
      fmt_number(
        columns = 2:5,
        decimals = 2
      )
  )
}

# Combine the app
shinyApp(ui, server)