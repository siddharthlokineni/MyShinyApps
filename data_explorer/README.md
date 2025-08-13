# Clinical Trial Data Explorer

A comprehensive Shiny dashboard for exploring clinical trial data in CDISC format.

## Features

- **Demographics**: Age and gender distribution analysis by treatment arm
- **Efficacy Analysis**: Time-course analysis with individual and mean profiles
- **PK/PD Analysis**: Pharmacokinetic profiles, AUC analysis, and dose proportionality
- **Safety Overview**: Adverse events and laboratory abnormalities
- **Data Tables**: Interactive CDISC domain tables (DM, VS, PC)

## Dependencies

This app requires the following R packages:

- `shiny` - Web application framework
- `shinydashboard` - Dashboard layout components
- `DT` - Interactive data tables
- `plotly` - Interactive plots
- `dplyr` - Data manipulation
- `ggplot2` - Plotting system
- `randomizr` - Randomization utilities

## Usage

1. Ensure you have R and the required packages installed
2. Run the app using: `shiny::runApp()`
3. Navigate through the different tabs to explore the data

## Data Structure

The app generates simulated CDISC-compliant data including:
- **DM Domain**: Demographics (age, sex, race, country, treatment arm)
- **VS Domain**: Vital signs (systolic blood pressure over time)
- **PC Domain**: Pharmacokinetics (drug concentration over time)

## Analysis Capabilities

### Efficacy Analysis
- Change from baseline analysis
- Statistical summaries by treatment arm
- Individual patient profiles
- Visit-specific comparisons

### PK Analysis
- Mean and individual concentration profiles
- AUC calculation using trapezoidal rule
- Cmax and Tmax analysis
- Dose proportionality assessment

### Safety Analysis
- Treatment-emergent adverse events
- Laboratory abnormalities
- Safety summary tables
