# DS400: Bayesian Statistics - Final Project

## ⚾ Baseball Pitch Analysis: Bayesian Strike Zone Predictor

<!-- Add CUH logo here -->
<!-- ![CUH Logo](path/to/cuh_logo.png) -->

### Project Description

This project implements a Bayesian strike zone predictor using Shiny. The application allows users to select a pitch type and target location in the strike zone, then provides Bayesian predictions about the likelihood of a strike call.

**Key Features:**
- Interactive Shiny application with clickable strike zone
- Bayesian logistic regression modeling
- Real-time strike probability predictions with credible intervals
- Posterior distribution visualization

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

<!-- Add links to each group member's GitHub profile -->
- [Your Name](https://github.com/yourusername)
- [Group Member 2](https://github.com/member2)
- [Group Member 3](https://github.com/member3)
- [Group Member 4](https://github.com/member4)

### Repository Setup

To fork and run this analysis:

1. **Fork this repository** by clicking the "Fork" button on GitHub
2. **Clone your fork:**
   ```bash
   git clone https://github.com/yourusername/DS400.git
   cd DS400
   ```
3. **Install required packages** (if not already installed):
   ```r
   install.packages(c("shiny", "ggplot2", "dplyr", "rstanarm", "bayesplot", "shinythemes"))
   ```
4. **Run the app:**
   ```r
   source("baseball_pitch_app.R")
   ```

The entire analysis can be run without editing any code - simply execute the commands above!