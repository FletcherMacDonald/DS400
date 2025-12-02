# Baseball Pitch Analysis Shiny App
# Bayesian Modeling of Pitch Outcomes

library(shiny)
library(ggplot2)
library(dplyr)
library(rstanarm)
library(bayesplot)
library(shinythemes)

# Define strike zone dimensions (in feet from center of plate)
# Standard strike zone: 17 inches wide, 1.5-3.5 feet high
STRIKE_ZONE_WIDTH <- 1.417  # 17 inches = 1.417 feet
STRIKE_ZONE_HEIGHT <- 2.0   # 1.5 to 3.5 feet = 2 feet height

# Generate synthetic pitch data for Bayesian modeling
set.seed(123)
n_pitches <- 1000

# Create realistic pitch data
pitch_data <- data.frame(
  pitch_type = sample(c("Fastball", "Curveball", "Slider", "Changeup", "Cutter"), 
                      n_pitches, replace = TRUE, 
                      prob = c(0.4, 0.15, 0.2, 0.15, 0.1)),
  plate_x = rnorm(n_pitches, 0, 0.8),  # Horizontal position (feet from center)
  plate_z = rnorm(n_pitches, 2.5, 0.6), # Vertical position (feet from ground)
  stringsAsFactors = FALSE
) %>%
  mutate(
    # Determine if pitch is in strike zone
    in_zone = abs(plate_x) <= STRIKE_ZONE_WIDTH/2 & 
              plate_z >= 1.5 & plate_z <= 3.5,
    # Strike probability depends on location and pitch type
    strike_prob = case_when(
      in_zone ~ 0.85,
      abs(plate_x) <= STRIKE_ZONE_WIDTH & plate_z >= 1.0 & plate_z <= 4.0 ~ 0.50,
      TRUE ~ 0.15
    ),
    # Adjust by pitch type
    strike_prob = case_when(
      pitch_type == "Fastball" ~ strike_prob * 1.05,
      pitch_type == "Curveball" ~ strike_prob * 0.95,
      pitch_type == "Slider" ~ strike_prob * 1.0,
      pitch_type == "Changeup" ~ strike_prob * 0.98,
      TRUE ~ strike_prob
    ),
    strike_prob = pmin(pmax(strike_prob, 0), 1),
    # Generate binary strike outcome
    is_strike = rbinom(n_pitches, 1, strike_prob),
    # Expected outcome (simplified)
    outcome = case_when(
      is_strike == 1 & in_zone ~ "Called Strike",
      is_strike == 1 & !in_zone ~ "Swinging Strike",
      is_strike == 0 & in_zone ~ "Ball (Missed Call)",
      TRUE ~ "Ball"
    )
  )

# Fit Bayesian logistic regression model
# Model: strike probability ~ pitch_type + location (plate_x, plate_z)
fit_model <- stan_glm(
  is_strike ~ pitch_type + plate_x + plate_z + I(plate_x^2) + I(plate_z^2) + 
              pitch_type:plate_x + pitch_type:plate_z,
  data = pitch_data,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 2.5),
  chains = 4,
  iter = 2000,
  seed = 123
)

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel(
    h1("Baseball Pitch Analysis: Bayesian Strike Zone Predictor", 
       align = "center", 
       style = "color: #2c3e50; margin-bottom: 20px;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h3("Pitch Selection", style = "color: #34495e;"),
      
      selectInput(
        "pitch_type",
        label = h4("Select Pitch Type:", style = "color: #2c3e50;"),
        choices = c("Fastball", "Curveball", "Slider", "Changeup", "Cutter"),
        selected = "Fastball"
      ),
      
      br(),
      
      h3("Strike Zone Target", style = "color: #34495e;"),
      
      h5("Click on the strike zone plot to select target location", 
         style = "color: #7f8c8d; font-style: italic;"),
      
      br(),
      
      h4("Selected Coordinates:", style = "color: #2c3e50;"),
      verbatimTextOutput("selected_coords"),
      
      br(),
      
      actionButton(
        "analyze",
        label = "Analyze Pitch",
        class = "btn-primary",
        style = "width: 100%; font-size: 16px; padding: 10px;"
      ),
      
      br(), br(),
      
      h4("Model Information", style = "color: #34495e;"),
      p("This app uses Bayesian logistic regression to predict strike probability 
        based on pitch type and location in the strike zone.",
        style = "font-size: 12px; color: #7f8c8d;")
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(12,
          h3("Interactive Strike Zone", align = "center"),
          plotOutput("strike_zone_plot", 
                     click = "zone_click",
                     height = "500px",
                     width = "100%")
        )
      ),
      
      br(),
      
      fluidRow(
        column(6,
          h3("Bayesian Predictions", align = "center"),
          wellPanel(
            h4("Strike Probability:", style = "color: #2c3e50;"),
            h2(textOutput("strike_prob"), align = "center", 
               style = "color: #27ae60; font-weight: bold;"),
            br(),
            h4("Predicted Outcome:", style = "color: #2c3e50;"),
            h3(textOutput("predicted_outcome"), align = "center",
               style = "color: #3498db; font-weight: bold;"),
            br(),
            h4("95% Credible Interval:", style = "color: #2c3e50;"),
            textOutput("credible_interval", container = h5)
          )
        ),
        column(6,
          h3("Posterior Distribution", align = "center"),
          plotOutput("posterior_plot", height = "400px")
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
          h3("Model Summary", align = "center"),
          verbatimTextOutput("model_summary")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store selected coordinates
  selected <- reactiveValues(
    plate_x = 0,
    plate_z = 2.5,
    clicked = FALSE
  )
  
  # Handle click on strike zone
  observeEvent(input$zone_click, {
    selected$plate_x <- input$zone_click$x
    selected$plate_z <- input$zone_click$y
    selected$clicked <- TRUE
  })
  
  # Display selected coordinates
  output$selected_coords <- renderText({
    if (selected$clicked) {
      paste0("X (horizontal): ", round(selected$plate_x, 3), " ft\n",
             "Z (vertical): ", round(selected$plate_z, 3), " ft")
    } else {
      "Click on strike zone to select location"
    }
  })
  
  # Strike zone plot
  output$strike_zone_plot <- renderPlot({
    # Create strike zone rectangle
    strike_zone <- data.frame(
      x = c(-STRIKE_ZONE_WIDTH/2, STRIKE_ZONE_WIDTH/2, 
            STRIKE_ZONE_WIDTH/2, -STRIKE_ZONE_WIDTH/2, -STRIKE_ZONE_WIDTH/2),
      y = c(1.5, 1.5, 3.5, 3.5, 1.5)
    )
    
    p <- ggplot() +
      # Strike zone outline
      geom_path(data = strike_zone, aes(x = x, y = y), 
                color = "black", size = 2, linetype = "solid") +
      # Strike zone fill
      geom_polygon(data = strike_zone, aes(x = x, y = y), 
                   fill = "lightblue", alpha = 0.2) +
      # Selected point
      geom_point(aes(x = selected$plate_x, y = selected$plate_z),
                 color = "red", size = 8, shape = 21, fill = "red", alpha = 0.7) +
      # Home plate
      annotate("rect", xmin = -0.708, xmax = 0.708, ymin = 0, ymax = 0.1,
               fill = "white", color = "black", size = 1) +
      # Labels
      labs(
        title = paste("Target Location for", input$pitch_type),
        subtitle = "Click anywhere to select target",
        x = "Horizontal Position (feet from center)",
        y = "Vertical Position (feet from ground)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")
      ) +
      xlim(-2, 2) +
      ylim(0, 5) +
      coord_fixed(ratio = 1)
    
    p
  })
  
  # Bayesian predictions
  predictions <- eventReactive(input$analyze, {
    if (!selected$clicked) {
      return(NULL)
    }
    
    # Create prediction data frame
    new_data <- data.frame(
      pitch_type = input$pitch_type,
      plate_x = selected$plate_x,
      plate_z = selected$plate_z
    )
    
    # Get posterior predictions
    posterior_pred <- posterior_predict(
      fit_model,
      newdata = new_data,
      draws = 1000
    )
    
    # Calculate strike probability
    strike_prob <- mean(posterior_pred)
    
    # Get credible interval
    ci <- quantile(posterior_pred, c(0.025, 0.975))
    
    # Predict outcome
    predicted_outcome <- if (strike_prob > 0.7) {
      if (abs(selected$plate_x) <= STRIKE_ZONE_WIDTH/2 && 
          selected$plate_z >= 1.5 && selected$plate_z <= 3.5) {
        "Called Strike"
      } else {
        "Swinging Strike"
      }
    } else if (strike_prob > 0.4) {
      "Borderline (Swing Dependent)"
    } else {
      "Ball"
    }
    
    list(
      strike_prob = strike_prob,
      credible_interval = ci,
      predicted_outcome = predicted_outcome,
      posterior_samples = as.numeric(posterior_pred)
    )
  })
  
  # Display strike probability
  output$strike_prob <- renderText({
    if (is.null(predictions())) {
      "Select location and click 'Analyze Pitch'"
    } else {
      paste0(round(predictions()$strike_prob * 100, 1), "%")
    }
  })
  
  # Display predicted outcome
  output$predicted_outcome <- renderText({
    if (is.null(predictions())) {
      ""
    } else {
      predictions()$predicted_outcome
    }
  })
  
  # Display credible interval
  output$credible_interval <- renderText({
    if (is.null(predictions())) {
      ""
    } else {
      ci <- predictions()$credible_interval
      paste0("[", round(ci[1] * 100, 1), "%, ", 
             round(ci[2] * 100, 1), "%]")
    }
  })
  
  # Posterior distribution plot
  output$posterior_plot <- renderPlot({
    if (is.null(predictions())) {
      return(NULL)
    }
    
    posterior_samples <- predictions()$posterior_samples
    
    ggplot(data.frame(prob = posterior_samples), aes(x = prob)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 30, fill = "#3498db", alpha = 0.7, 
                     color = "white") +
      geom_density(color = "#2c3e50", size = 1.5) +
      geom_vline(xintercept = mean(posterior_samples), 
                 color = "red", linetype = "dashed", size = 1) +
      labs(
        title = "Posterior Distribution of Strike Probability",
        x = "Strike Probability",
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold")
      ) +
      xlim(0, 1)
  })
  
  # Model summary
  output$model_summary <- renderPrint({
    summary(fit_model)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

