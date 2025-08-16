# MyShinyApps

A collection of R Shiny applications for clinical trial data analysis and exploration.

## Deployed Applications

All applications are deployed and accessible at the following URLs:

1. [Teal Dashboard](https://sid-lokineni.shinyapps.io/teal_dashboard/) - Interactive analysis of clinical trial data
2. [Safety Dashboard](https://sid-lokineni.shinyapps.io/safety-dashboard-v1/) - Clinical Safety Dashboard
3. [Biostat Dashboard](https://sid-lokineni.shinyapps.io/biostat_dashboard/) - Clinical Trial Survival Analysis
4. [Patient Journey](https://sid-lokineni.shinyapps.io/Patient_Journey/) - Enhanced Clinical Trial Patient Journey Analysis
5. [Data Explorer](https://sid-lokineni.shinyapps.io/data_explorer/) - Clinical Trial Data Explorer

## Project Structure

```
MyShinyApps/
├── README.md                    # Main project documentation
├── biostat_dashboard/          # Clinical trial survival analysis app
├── data_explorer/             # Clinical trial data explorer app
├── PV/                       # Safety/Pharmacovigilance dashboard
├── sankey/                  # Patient Journey visualization app
└── teal_dashboard/         # Teal-based CDISC analysis app
```

## Applications

### 1. Teal Dashboard (`teal_dashboard/`)

An interactive CDISC data analysis dashboard built with teal, tern, and rtables featuring:
- Demographics Tables
- Adverse Events Analysis
- Laboratory Data Analysis
- Vital Signs Summary
- Interactive Data Explorer
- Variable Browser
- Report Generation

### 2. Safety Dashboard (`PV/`)

A comprehensive pharmacovigilance dashboard featuring:
- AE Trends Over Time
- Signal Detection
- Risk Assessment
- Regulatory Reporting
- Study Comparison
- Causality Assessment

### 3. Biostat Dashboard (`biostat_dashboard/`)

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

### 4. Patient Journey (`sankey/`)

A Sankey diagram-based visualization tool for patient flow analysis:
- Demographic flow visualization
- Treatment pathways
- Adverse event tracking
- Response monitoring
- Outcome analysis

### 5. Data Explorer (`data_explorer/`)

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

### Running the Apps Locally

Each app can be run locally using:

```r
setwd("app_directory")  # Replace with specific app directory
shiny::runApp()
```

## License

This project is open source and available under the MIT License.