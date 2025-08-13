# Biostat Dashboard

A comprehensive Shiny dashboard for clinical trial data analysis and survival analysis.

## Features

- **Data Overview**: Summary statistics and interactive data tables
- **Survival Analysis**: Kaplan-Meier plots and Cox regression results
- **Demographics**: Age, sex, stage, and ECOG distribution visualizations
- **Forest Plot**: Interactive hazard ratio forest plots with customization options

## Dependencies

This app requires the following R packages:

- `shiny` - Web application framework
- `shinydashboard` - Dashboard layout components
- `DT` - Interactive data tables
- `plotly` - Interactive plots
- `dplyr` - Data manipulation
- `ggplot2` - Plotting system
- `forestplot` - Forest plot creation
- `survival` - Survival analysis
- `survminer` - Survival analysis visualization

## Usage

1. Ensure you have R and the required packages installed
2. Run the app using: `shiny::runApp()`
3. Navigate through the different tabs to explore the data

## Data

The app generates simulated clinical trial data including:
- Demographics (age, sex, race, ECOG, cancer stage, biomarker status)
- Overall survival data with treatment effects
- Stratification factors for subgroup analysis
