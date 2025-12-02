# DS400: Bayesian Statistics

## Final Project: Baseball Pitch Analysis

### Project Description

This project implements a Bayesian strike zone predictor using Shiny. The application allows users to select a pitch type and target location in the strike zone, then provides Bayesian predictions about the likelihood of a strike.

### Files

- `baseball_pitch_app.R` - Main Shiny application
- `baseball_pitch_app.qmd` - Quarto document with analysis and documentation

### Running the Application

To run the Shiny app:

```r
# Option 1: Source the file
source("baseball_pitch_app.R")

# Option 2: Use runApp
shiny::runApp("baseball_pitch_app.R")
```

### Features

1. **Pitch Type Selection**: Choose from Fastball, Curveball, Slider, Changeup, or Cutter
2. **Interactive Strike Zone**: Click on the strike zone plot to select target location
3. **Bayesian Predictions**: 
   - Strike probability with 95% credible intervals
   - Predicted outcome (Strike/Ball)
   - Posterior distribution visualization

### Bayesian Methods

The app uses Bayesian logistic regression (`rstanarm::stan_glm`) to model strike probability as a function of:
- Pitch type
- Horizontal location (plate_x)
- Vertical location (plate_z)
- Quadratic terms and interactions

### Dependencies

```r
library(shiny)
library(ggplot2)
library(dplyr)
library(rstanarm)
library(bayesplot)
library(shinythemes)
```

### Data

The application uses synthetic pitch data generated to demonstrate Bayesian modeling principles. In a production setting, this would be replaced with real MLB pitch data.

### Contributors

[Add contributor GitHub profiles here]