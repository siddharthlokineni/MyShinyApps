# MyShinyApps

A collection of R Shiny applications for clinical trial data analysis and exploration.

## Project Structure

```
MyShinyApps/
├── README.md                    # Main project documentation
├── biostat_dashboard/          # Clinical trial survival analysis app
│   └── README.md              # App-specific documentation
└── data_explorer/             # Clinical trial data explorer app
    └── README.md              # App-specific documentation
```

## Applications

### 1. Biostat Dashboard (`biostat_dashboard/`)

A comprehensive dashboard for clinical trial survival analysis featuring:
- Kaplan-Meier survival curves
- Cox proportional hazards regression
- Interactive forest plots
- Demographics analysis
- Treatment effect visualization

**Key Features:**
- Survival analysis with customizable parameters
- Hazard ratio forest plots with multiple themes
- Comprehensive statistical summaries
- Interactive data exploration

### 2. Data Explorer (`data_explorer/`)

A CDISC-compliant clinical trial data exploration tool featuring:
- Demographics analysis
- Efficacy endpoint analysis
- Pharmacokinetic (PK) analysis
- Safety data overview
- Interactive data tables

**Key Features:**
- Time-course efficacy analysis
- PK parameter calculation (AUC, Cmax, Tmax)
- Dose proportionality assessment
- Safety event monitoring
- CDISC domain data exploration

## Getting Started

### Prerequisites

- R version 4.3.0 or higher
- RStudio (recommended)
- Required R packages (see individual app README files)

### Installation

1. Clone this repository
2. Navigate to the specific app directory
3. Install required packages
4. Run the app using `shiny::runApp()`

### Running the Apps

**Biostat Dashboard:**
```r
setwd("biostat_dashboard")
shiny::runApp()
```

**Data Explorer:**
```r
setwd("data_explorer")
shiny::runApp()
```

## Data Sources

Both applications generate simulated clinical trial data for demonstration purposes. The data structures are designed to mimic real-world clinical trial datasets and can be easily adapted for actual data analysis.

## Contributing

Feel free to enhance these applications by:
- Adding new analysis modules
- Improving the user interface
- Optimizing performance
- Adding new visualization options

## License

This project is open source and available under the MIT License.
