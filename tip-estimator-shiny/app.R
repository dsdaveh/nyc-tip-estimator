library(shiny)
library(bslib)
library(ggplot2)
library(lubridate)
library(httr)
library(jsonlite)

# Define NYC boroughs
nyc_boroughs <- c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")

# Function to get tip estimates from API (placeholder)
get_tip_estimates <- function(day_of_week, hour_of_day, borough) {
  # In a real app, this would make an API call to xyz.com/tip_estimate
  # For now, return mock data
  # Normally would be:
  # response <- POST("xyz.com/tip_estimate", 
  #                 body = list(day = day_of_week, hour = hour_of_day, borough = borough),
  #                 encode = "json")
  # result <- content(response, "parsed")
  
  # Mock data for development
  # Returns a list with 10%, 50%, and 90% percentiles (in that order)
  set.seed(as.numeric(as.POSIXct(Sys.time())) %% 10000)  # Different seed based on time
  
  # Generate more realistic tip estimates based on the inputs
  base_tip <- 5.00
  
  # Weekend effect
  if (day_of_week %in% c("Saturday", "Sunday")) {
    base_tip <- base_tip * 1.2
  }
  
  # Rush hour effect
  if (hour_of_day >= 7 && hour_of_day <= 9 || hour_of_day >= 16 && hour_of_day <= 19) {
    base_tip <- base_tip * 1.15
  }
  
  # Late night effect
  if (hour_of_day >= 22 || hour_of_day <= 4) {
    base_tip <- base_tip * 1.3
  }
  
  # Borough effect
  borough_multipliers <- c(
    "Manhattan" = 1.3,
    "Brooklyn" = 1.1,
    "Queens" = 1.0,
    "Bronx" = 0.9,
    "Staten Island" = 0.85
  )
  
  base_tip <- base_tip * borough_multipliers[borough]
  
  # Generate the percentiles with some variability
  p90 <- round(base_tip * 0.7 + runif(1, -0.5, 0.5), 2)
  p50 <- round(base_tip + runif(1, -0.5, 0.5), 2)
  p10 <- round(base_tip * 1.3 + runif(1, -0.5, 0.5), 2)
  
  return(list(p10 = p10, p50 = p50, p90 = p90))
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
      # Add the markers for each percentile
      geom_point(color = "blue", size = 5) +
      # Add special marker for median (50%)
      geom_segment(aes(x = 0.7, xend = 1.3, y = 2, yend = 2), 
                   color = "red", size = 1.5) +
      # Add labels
      geom_text(aes(label = paste0("$", value, " (", percentile, ")")), 
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
