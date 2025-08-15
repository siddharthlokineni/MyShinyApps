# Modern Teal Dashboard with tern, rtables, and Random CDISC Data
# Following latest teal framework documentation from insightsengineering.github.io/teal/latest-tag/
# Compatible with R version 4.1.3 and below

# Load required libraries
library(teal)
library(teal.modules.clinical)
library(teal.modules.general)
library(teal.data)
library(teal.transform)  # Added for data_extract_spec
library(tern)
library(rtables)
library(dplyr)

# Create teal_data object with reproducible code using latest API
app_data <- within(teal_data(), {
  # Generate example CDISC datasets using teal.data
  ADSL <- example_cdisc_data("ADSL")
  ADAE <- example_cdisc_data("ADAE") 
  ADLB <- example_cdisc_data("ADLB")
  ADVS <- example_cdisc_data("ADVS")
  ADTTE <- example_cdisc_data("ADTTE")
  ADRS <- example_cdisc_data("ADRS")
})

# Set join keys for CDISC data using names() instead of deprecated datanames()
join_keys(app_data) <- default_cdisc_join_keys[names(app_data)]

# Create teal application
app <- init(
  data = app_data,
  modules = modules(
    
    # Demographics and Baseline Characteristics
    tm_t_summary(
      label = "Demographics Table",
      dataname = "ADSL",
      arm_var = choices_selected(
        variable_choices("ADSL", c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      summarize_vars = choices_selected(
        variable_choices("ADSL", c("AGE", "SEX", "RACE", "ETHNIC", "COUNTRY")),
        selected = c("AGE", "SEX", "RACE")
      ),
      useNA = "ifany"
    ),
    
    # Adverse Events Analysis
    tm_t_events_summary(
      label = "Adverse Events Summary",
      dataname = "ADAE",
      parentname = "ADSL",
      arm_var = choices_selected(
        variable_choices("ADSL", c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      llt = choices_selected(
        variable_choices("ADAE", c("AETERM", "AEDECOD")),
        selected = "AEDECOD"
      ),
      add_total = TRUE
    ),
    
    # 2. Detailed SOC → Preferred Terms breakdown
    # Hierarchical AE Table: SOC with nested Preferred Terms
    tm_t_events(
      label = "Adverse Events by SOC and Preferred Terms",
      dataname = "ADAE",
      parentname = "ADSL",
      arm_var = choices_selected(
        variable_choices("ADSL", c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      hlt = choices_selected(
        variable_choices("ADAE", c("AESOC")),
        selected = "AESOC"
      ),
      llt = choices_selected(
        variable_choices("ADAE", c("AEDECOD")),
        selected = "AEDECOD"
      ),
      add_total = TRUE,
      drop = TRUE,
      prune_freq = 0.05,  # Only show events occurring in >5% of patients
      prune_diff = 0      # Show all differences
    ),
    
    # Laboratory Analysis
    tm_t_summary_by(
      label = "Laboratory Summary",
      dataname = "ADLB",
      parentname = "ADSL",
      arm_var = choices_selected(
        variable_choices("ADSL", c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      by_vars = choices_selected(
        variable_choices("ADLB", c("PARAM", "AVISIT")),
        selected = "PARAM"
      ),
      summarize_vars = choices_selected(
        variable_choices("ADLB", c("AVAL", "CHG")),
        selected = "AVAL"
      ),
      useNA = "ifany"
    ),
    
    # Laboratory Values Over Time - Using scatterplot for flexibility
    tm_g_scatterplot(
      label = "Laboratory Values Over Time",
      x = data_extract_spec(
        dataname = "ADLB",
        select = select_spec(
          label = "Select X variable:",
          choices = variable_choices("ADLB", c("ADY", "AVISITN")),
          selected = "ADY",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      y = data_extract_spec(
        dataname = "ADLB", 
        select = select_spec(
          label = "Select Y variable:",
          choices = variable_choices("ADLB", c("AVAL", "CHG")),
          selected = "AVAL",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      color_by = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Color by:",
          choices = variable_choices("ADSL", c("ARM", "SEX", "RACE")),
          selected = "ARM",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      row_facet = data_extract_spec(
        dataname = "ADLB",
        select = select_spec(
          label = "Row facet by:",
          choices = variable_choices("ADLB", c("PARAM", "AVISIT")),
          selected = NULL,
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      col_facet = data_extract_spec(
        dataname = "ADLB",
        select = select_spec(
          label = "Column facet by:",
          choices = variable_choices("ADLB", c("PARAM", "PARAMCD")),
          selected = "PARAM",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      plot_height = c(600, 200, 2000),
      plot_width = NULL,
      alpha = c(1, 0, 1),
      size = c(1, 1, 12),
      shape = "circle"
    ),
    
    # Vital Signs Summary
    tm_t_summary_by(
      label = "Vital Signs Summary", 
      dataname = "ADVS",
      parentname = "ADSL",
      arm_var = choices_selected(
        variable_choices("ADSL", c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      by_vars = choices_selected(
        variable_choices("ADVS", c("PARAM", "AVISIT")),
        selected = "PARAM"
      ),
      summarize_vars = choices_selected(
        variable_choices("ADVS", c("AVAL", "CHG")),
        selected = "AVAL"
      ),
      useNA = "ifany"
    ),
    
    # General Data Exploration
    tm_data_table(
      label = "Data Explorer",
      variables_selected = list(
        ADSL = c("USUBJID", "ARM", "AGE", "SEX", "RACE"),
        ADAE = c("USUBJID", "AETERM", "AESEV", "AESER"),
        ADLB = c("USUBJID", "PARAM", "AVAL", "AVISIT"),
        ADVS = c("USUBJID", "PARAM", "AVAL", "AVISIT"),
        ADTTE = c("USUBJID", "PARAM", "AVAL", "CNSR"),
        ADRS = c("USUBJID", "PARAM", "AVAL", "AVALC")
      )
    ),
    
    # Variable Browser
    tm_variable_browser(
      label = "Variable Browser",
      datanames = "all"
    )
  ),
  
  # No filters for now to avoid column selection issues
  # filter = teal_slices(
  #   teal_slice(dataname = "ADSL", varname = "SAFFL", selected = "Y"),
  #   teal_slice(dataname = "ADAE", varname = "SAFFL", selected = "Y"),
  #   teal_slice(dataname = "ADVS", varname = "SAFFL", selected = "Y"),
  #   module_specific = TRUE
  # )
) %>%
  # Application customization
  modify_title("CDISC Clinical Data Analysis Dashboard") %>%
  modify_header(
    tags$div(
      style = "background: linear-gradient(90deg, #008080, #20B2AA); color: white; padding: 15px; margin: -15px -15px 15px -15px;",
      h1("Clinical Data Analysis Dashboard", 
         style = "margin: 0; font-size: 28px; font-weight: 300;"),
      p("Interactive analysis of CDISC clinical trial data using teal, tern, and rtables", 
        style = "margin: 8px 0 0 0; font-size: 14px; opacity: 0.9;")
    )
  ) %>%
  modify_footer(
    tags$div(
      style = "text-align: center; padding: 10px; color: #666; border-top: 1px solid #ddd; margin-top: 20px;",
      tags$p(
        "Built with ",
        tags$a("teal", href = "https://insightsengineering.github.io/teal/latest-tag/", 
               target = "_blank", style = "color: #008080;"),
        ", ",
        tags$a("tern", href = "https://insightsengineering.github.io/tern/", 
               target = "_blank", style = "color: #008080;"),
        ", and ",
        tags$a("rtables", href = "https://insightsengineering.github.io/rtables/", 
               target = "_blank", style = "color: #008080;"),
        " | Powered by the ",
        tags$a("pharmaverse", href = "https://pharmaverse.org/", 
               target = "_blank", style = "color: #008080;"),
        style = "margin: 0; font-size: 12px;"
      )
    )
  )

# Debug: Check what's actually in the datasets
cat("\n=== DEBUGGING DATASET CONTENTS ===\n")

# Check ADLB structure
cat("ADLB Dataset Structure:\n")
cat("Column names:\n")
print(names(app_data[["ADLB"]]))

cat("\nADLB dimensions:\n")
print(dim(app_data[["ADLB"]]))

cat("\nUnique PARAMCD values in ADLB:\n")
print(unique(app_data[["ADLB"]]$PARAMCD))

cat("\nUnique PARAM values in ADLB:\n")
print(unique(app_data[["ADLB"]]$PARAM))

cat("\nFirst few rows of ADLB:\n")
print(head(app_data[["ADLB"]][c("USUBJID", "PARAMCD", "PARAM", "AVAL", "AVISIT")]))

cat("\n=== END DEBUGGING ===\n")

cat("Session Info:\n")
print(sessionInfo())

cat("\nDatasets included:\n")
for (name in names(app_data)) {
  dataset_size <- nrow(app_data[[name]])
  cat(sprintf("- %s: %d records\n", name, dataset_size))
}

cat("\nAnalysis Modules:\n")
cat("- Demographics Table (tern/rtables)\n")
cat("- Adverse Events Summary\n")
cat("- Adverse Events by Body System\n")
cat("- Adverse Events by Preferred Terms\n")
cat("- Adverse Events by System Organ Class (SOC)\n")
cat("- Detailed SOC Analysis by Treatment Arm\n")
cat("- Laboratory Summary by Visit\n")
cat("- Laboratory Values Over Time Plot\n")
cat("- Vital Signs Summary\n")
cat("- Data Explorer\n")
cat("- Variable Browser\n")

cat("\nFeatures:\n")
cat("- Reproducible code tracking\n")
cat("- Interactive filtering with module-specific filters\n")
cat("- Report generation capabilities\n")
cat("- Modern teal design with custom styling\n")
cat("- CDISC-compliant analysis workflows\n")

# Run the application
cat("\nStarting teal application...\n")

# Start the app with automatic browser opening
shinyApp(app$ui, app$server, options = list(launch.browser = TRUE))

# Example of standalone tern/rtables code using the same data
cat("\n=== Example tern/rtables Output ===\n")

# Extract ADSL data for demonstration
adsl_data <- app_data[["ADSL"]]

# Create demographics table using tern/rtables
demo_table <- basic_table() %>%
  split_cols_by("ARM") %>%
  add_overall_col("All Patients") %>%
  summarize_row_groups() %>%
  split_rows_by("SEX") %>%
  summarize_row_groups() %>%
  analyze_vars(
    vars = c("AGE"),
    .stats = c("n", "mean_sd", "median", "range"),
    .formats = list(
      n = "xx",
      mean_sd = "xx.x (xx.x)",
      median = "xx.x", 
      range = "xx.x - xx.x"
    )
  ) %>%
  analyze_vars(
    vars = c("RACE"),
    .stats = c("count_fraction"),
    .formats = list(count_fraction = "xx (xx.x%)")
  ) %>%
  build_table(adsl_data)

print(demo_table)