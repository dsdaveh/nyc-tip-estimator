library(shiny)
library(bslib)
library(ggplot2)
library(lubridate)
library(httr)
library(jsonlite)

# API configuration
api_token <- Sys.getenv("NYC_TIPS_API_KEY")
if (api_token == "") {
  stop(
    "API key not found. Please set the NYC_TIPS_API_KEY environment variable.\n",
    "You can do this by:\n",
    "1. Creating a .Renviron file in your project directory\n",
    "2. Adding the line: NYC_TIPS_API_KEY=your_api_key_here\n",
    "3. Restart your R session"
  )
}
api_base_url <- "https://connect-oha.acuna.cloud/nyc-tips-api"

# Define function to get NYC boroughs from API
get_nyc_boroughs <- function() {
  tryCatch({
    response <- GET(
      paste0(api_base_url, "/boroughs"),
      add_headers(Authorization = paste("Key", api_token))
    )
    if (status_code(response) == 200) {
      boroughs <- fromJSON(rawToChar(response$content))
      return(boroughs)
    } else {
      # Fallback to default boroughs if API fails
      warning(sprintf("API call failed with status %d. Response: %s", 
                     status_code(response), 
                     rawToChar(response$content)))
      return(c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"))
    }
  }, error = function(e) {
    # Fallback to default boroughs if API fails
    warning("API call error: ", e$message)
    return(c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"))
  })
}

# Get boroughs from API
nyc_boroughs <- get_nyc_boroughs()

# Function to convert hour to period
hour_to_period <- function(hour) {
  if (hour >= 22 || hour < 5) {
    return("Late Night")
  } else if (hour >= 5 && hour < 12) {
    return("Morning")
  } else if (hour >= 12 && hour < 17) {
    return("Afternoon")
  } else {
    return("Evening")
  }
}

# Function to get tip estimates from API
get_tip_estimates <- function(day_of_week, hour_of_day, borough) {
  period <- hour_to_period(hour_of_day)
  
  # Construct API URL with query parameters
  base_url <- paste0(api_base_url, "/predict")
  body <- list(
    day_of_week = day_of_week,
    period = period,
    borough = borough
  )
  
  tryCatch({
    message(sprintf("Making API request with parameters: day=%s, period=%s, borough=%s", 
                   day_of_week, period, borough))
    response <- POST(
      base_url,
      add_headers(Authorization = paste("Key", api_token)),
      body = body,
      encode = "json"
    )
    
    if (status_code(response) == 200) {
      result <- fromJSON(rawToChar(response$content))
      message("API response: ", rawToChar(response$content))
      
      # Check if we got the expected fields
      if (!all(c("q10", "q50", "q90") %in% names(result))) {
        warning("API response missing expected fields. Response: ", rawToChar(response$content))
        return(list(
          p10 = 3.00,
          p50 = 5.00,
          p90 = 7.00
        ))
      }
      
      return(list(
        p10 = round(as.numeric(result$q10), 2),
        p50 = round(as.numeric(result$q50), 2),
        p90 = round(as.numeric(result$q90), 2)
      ))
    } else {
      warning(sprintf("API call failed with status %d. Response: %s", 
                     status_code(response), 
                     rawToChar(response$content)))
      return(list(
        p10 = 3.00,
        p50 = 5.00,
        p90 = 7.00
      ))
    }
  }, error = function(e) {
    warning("API call error: ", e$message)
    return(list(
      p10 = 3.00,
      p50 = 5.00,
      p90 = 7.00
    ))
  })
}

# UI
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # Custom CSS for mobile-friendly experience
  tags$head(
    tags$style(HTML("
      @media (max-width: 768px) {
        .card {
          margin-bottom: 15px;
        }
        .form-label {
          font-size: 1.1rem;
        }
        .btn {
          width: 100%;
          padding: 12px;
          font-size: 1.1rem;
        }
        .shiny-plot-output {
          touch-action: pan-y pinch-zoom;
        }
        body {
          padding-top: 10px;
          padding-bottom: 20px;
        }
      }
    "))
  ),
  
  # Navbar with title
  navset_card_tab(
    title = "NYC Taxi Tip Estimator",
    nav_panel(
      title = "Estimate Tips",
      
      # Input section
      card(
        card_header("Input Parameters"),
        card_body(
          selectInput(
            "day_of_week",
            "Day of Week",
            choices = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                        "Friday", "Saturday", "Sunday"),
            selected = weekdays(Sys.Date()),
            width = "100%"
          ),
          
          sliderInput(
            "hour_of_day",
            "Hour of Day (24h format)",
            min = 0,
            max = 23,
            value = hour(now()),
            step = 1,
            width = "100%"
          ),
          
          textInput(
            "hour_display",
            "Selected Hour",
            value = sprintf("%02d:00", hour(now())),
            width = "100%"
          ),
          
          selectInput(
            "borough",
            "NYC Borough",
            choices = nyc_boroughs,
            selected = nyc_boroughs[1],
            width = "100%"
          ),
          
          actionButton(
            "estimate_btn", 
            "Estimate Tips",
            class = "btn-primary",
            width = "100%"
          )
        )
      ),
      
      # Output section
      card(
        card_header("Tip Estimates"),
        card_body(
          uiOutput("tip_display")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Make hour display read-only
  observe({
    updateTextInput(session, "hour_display", value = sprintf("%02d:00", input$hour_of_day))
  })
  
  # Disable interaction with hour_display
  shinyjs::runjs("document.getElementById('hour_display').readOnly = true;")
  
  # Reactive value to store tip estimates
  tip_estimates <- reactiveVal(NULL)
  
  # Get estimates when button is clicked
  observeEvent(input$estimate_btn, {
    estimates <- get_tip_estimates(
      input$day_of_week,
      input$hour_of_day,
      input$borough
    )
    tip_estimates(estimates)
  })
  
  # Update display when estimates change
  output$tip_display <- renderUI({
    est <- tip_estimates()
    if (is.null(est)) {
      return(p("Click 'Estimate Tips' to see your results", class = "text-center fs-5 text-muted mt-4"))
    }
    
    # Create plot data for vertical bar
    tip_data <- data.frame(
      percentile = c("90%", "50%", "10%"),
      value = c(est$p90, est$p50, est$p10),
      y_pos = c(1, 2, 3)
    )
    
    # Create the ggplot
    p <- ggplot(tip_data, aes(x = 1, y = y_pos)) +
      # Create the vertical bar
      geom_segment(aes(x = 1, xend = 1, y = 1, yend = 3), 
                   color = "grey", size = 10, alpha = 0.3) +
      # Add the markers for each percentile, with 50% being an open point
      geom_point(data = tip_data[tip_data$percentile != "50%",], color = "blue", size = 5) +
      geom_point(data = tip_data[tip_data$percentile == "50%",], color = "blue", size = 5, shape = 1, stroke = 2) +
      # Add labels with 2 decimal places
      geom_text(aes(label = sprintf("$%.2f (%s)", value, percentile)), 
                hjust = -0.2, size = 5) +
      # Theme adjustments
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(20, 20, 20, 60),
        plot.title = element_text(hjust = 0.5, size = 14)
      ) +
      # Set limits to make it look better
      xlim(0.5, 3) +
      ylim(0.5, 3.5) +
      # Add a title
      labs(title = paste0("Tip Estimates for ", input$borough))
    
    # Render plot in a div with responsive height
    tagList(
      div(
        class = "d-flex flex-column align-items-center",
        div(
          class = "mb-3",
          h3("Estimated Tip Range", class = "text-center")
        ),
        div(
          class = "w-100",
          style = "height: 300px;",
          renderPlot({
            p
          }, height = 300, res = 96)
        ),
        div(
          class = "mt-3 text-center",
          p(
            "Based on ", tags$strong(input$day_of_week), 
            " at ", tags$strong(sprintf("%02d:00", input$hour_of_day)),
            " in ", tags$strong(input$borough)
          )
        )
      )
    )
  })
}

# Run the app
shinyApp(ui, server)
