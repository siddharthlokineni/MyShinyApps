library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(forestplot)
library(survival)
library(survminer)

# Generate more complex clinical trial data
generate_advanced_data <- function() {
  set.seed(456)
  n <- 300
  
  # Enhanced demographics with stratification factors
  dm_data <- data.frame(
    USUBJID = paste0("SUBJ-", sprintf("%04d", 1:n)),
    STUDYID = "ONCOLOGY-001",
    ARM = sample(c("Control", "Experimental"), n, replace = TRUE, prob = c(0.5, 0.5)),
    AGE = round(rnorm(n, 62, 10)),
    SEX = sample(c("M", "F"), n, replace = TRUE, prob = c(0.55, 0.45)),
    RACE = sample(c("WHITE", "BLACK", "ASIAN", "OTHER"), n, replace = TRUE, prob = c(0.75, 0.12, 0.10, 0.03)),
    ECOG = sample(c(0, 1, 2), n, replace = TRUE, prob = c(0.4, 0.5, 0.1)),
    STAGE = sample(c("I", "II", "III", "IV"), n, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.3)),
    BIOMARKER = sample(c("Positive", "Negative", "Unknown"), n, replace = TRUE, prob = c(0.3, 0.6, 0.1))
  )
  
  # Time-to-event data (Overall Survival)
  os_data <- data.frame()
  for(i in 1:n) {
    subj <- dm_data$USUBJID[i]
    arm <- dm_data$ARM[i]
    age <- dm_data$AGE[i]
    ecog <- dm_data$ECOG[i]
    stage <- dm_data$STAGE[i]
    biomarker <- dm_data$BIOMARKER[i]
    
    # Hazard ratio based on treatment and covariates
    hr_treatment <- ifelse(arm == "Experimental", 0.65, 1.0)  # 35% reduction in hazard
    hr_age <- exp((age - 62) * 0.02)  # Age effect
    hr_ecog <- case_when(ecog == 0 ~ 0.8, ecog == 1 ~ 1.0, ecog == 2 ~ 1.4)
    hr_stage <- case_when(stage == "I" ~ 0.5, stage == "II" ~ 0.7, stage == "III" ~ 1.0, stage == "IV" ~ 1.6)
    hr_biomarker <- case_when(biomarker == "Positive" ~ 0.7, biomarker == "Negative" ~ 1.0, biomarker == "Unknown" ~ 1.1)
    
    # Combined hazard rate
    lambda <- 0.05 * hr_treatment * hr_age * hr_ecog * hr_stage * hr_biomarker
    
    # Generate survival time
    os_time <- rexp(1, lambda)
    
    # Censoring (administrative censoring at 36 months, random censoring)
    censor_time <- 36
    random_censor <- runif(1, 12, 48)  # Random censoring between 12-48 months
    actual_censor_time <- min(censor_time, random_censor)
    
    os_event <- ifelse(os_time <= actual_censor_time, 1, 0)
    os_time_final <- min(os_time, actual_censor_time)
    
    # Add to OS data
    os_data <- rbind(os_data, data.frame(
      USUBJID = subj,
      OS_TIME = os_time_final,
      OS_EVENT = os_event,
      OS_CENSOR = actual_censor_time
    ))
  }
  
  # Merge demographics with OS data
  final_data <- merge(dm_data, os_data, by = "USUBJID")
  
  return(final_data)
}

# Test the function
test_data <- generate_advanced_data()
print(head(test_data))
print(paste("Total subjects:", nrow(test_data)))
print(paste("Events:", sum(test_data$OS_EVENT)))
print(paste("Censored:", sum(test_data$OS_EVENT == 0)))

# Shiny App UI
ui <- dashboardPage(
  dashboardHeader(title = "Clinical Trial Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview", icon = icon("table")),
      menuItem("Survival Analysis", tabName = "survival", icon = icon("chart-line")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Forest Plot", tabName = "forest", icon = icon("tree"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Data Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          box(title = "Dataset Summary", width = 12,
            verbatimTextOutput("summary_stats")
          )
        ),
        fluidRow(
          box(title = "Data Table", width = 12,
            DTOutput("data_table")
          )
        )
      ),
      
      # Survival Analysis Tab
      tabItem(tabName = "survival",
        fluidRow(
          box(title = "Kaplan-Meier Plot", width = 12,
            plotlyOutput("km_plot")
          )
        ),
        fluidRow(
          box(title = "Cox Regression Results", width = 12,
            verbatimTextOutput("cox_results")
          )
        )
      ),
      
      # Demographics Tab
      tabItem(tabName = "demographics",
        fluidRow(
          box(title = "Age Distribution", width = 6,
            plotlyOutput("age_plot")
          ),
          box(title = "Sex Distribution", width = 6,
            plotlyOutput("sex_plot")
          )
        ),
        fluidRow(
          box(title = "Stage Distribution", width = 6,
            plotlyOutput("stage_plot")
          ),
          box(title = "ECOG Distribution", width = 6,
            plotlyOutput("ecog_plot")
          )
        )
      ),
      
      # Forest Plot Tab
      tabItem(tabName = "forest",
        fluidRow(
          box(title = "Forest Plot Customization", width = 12,
            column(width = 3,
              selectInput("forest_variables", "Select Variables:", 
                         choices = c("All Variables", "Treatment Only", "Demographics Only", "Clinical Only"),
                         selected = "All Variables")
            ),
            column(width = 3,
              selectInput("forest_theme", "Plot Theme:", 
                         choices = c("Default", "Professional", "Colorful", "Minimal"),
                         selected = "Default")
            ),
            column(width = 3,
              numericInput("forest_alpha", "Significance Level:", 
                          value = 0.05, min = 0.001, max = 0.1, step = 0.001)
            ),
            column(width = 3,
              actionButton("forest_refresh", "Refresh Plot", class = "btn-primary")
            )
          )
        ),
        fluidRow(
          box(title = "Hazard Ratio Forest Plot", width = 12,
            plotOutput("forest_plot", height = "600px")
          )
        ),
        fluidRow(
          box(title = "Detailed Hazard Ratio Results", width = 12,
            DTOutput("forest_table")
          )
        )
      )
    )
  )
)

# Shiny App Server
server <- function(input, output, session) {
  
  # Generate data
  data <- reactive({
    generate_advanced_data()
  })
  
  # Event handler for forest plot refresh
  observeEvent(input$forest_refresh, {
    # This will trigger re-rendering of the forest plot and table
    # The reactive nature of Shiny will handle the updates automatically
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    dat <- data()
    cat("Clinical Trial Dataset Summary\n")
    cat("=============================\n")
    cat("Total Subjects:", nrow(dat), "\n")
    cat("Treatment Arms:", paste(unique(dat$ARM), collapse = ", "), "\n")
    cat("Events:", sum(dat$OS_EVENT), "\n")
    cat("Censored:", sum(dat$OS_EVENT == 0), "\n")
    cat("Median OS (months):", round(median(dat$OS_TIME), 2), "\n")
    cat("Age Range:", min(dat$AGE), "-", max(dat$AGE), "years\n")
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(data(), 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top")
  })
  
  # Kaplan-Meier plot
  output$km_plot <- renderPlotly({
    dat <- data()
    
    # Fit survival model
    fit <- survfit(Surv(OS_TIME, OS_EVENT) ~ ARM, data = dat)
    
    # Create plot
    p <- ggsurvplot(fit, data = dat, 
                    pval = TRUE, conf.int = TRUE,
                    risk.table = TRUE, 
                    palette = c("#E7B800", "#2E9FDF"),
                    title = "Kaplan-Meier Survival Curves by Treatment Arm")
    
    ggplotly(p$plot)
  })
  
  # Cox regression results
  output$cox_results <- renderPrint({
    dat <- data()
    
    # Fit Cox model
    cox_model <- coxph(Surv(OS_TIME, OS_EVENT) ~ ARM + AGE + SEX + ECOG + STAGE + BIOMARKER, data = dat)
    
    cat("Cox Proportional Hazards Model\n")
    cat("==============================\n")
    print(summary(cox_model))
  })
  
  # Age distribution
  output$age_plot <- renderPlotly({
    dat <- data()
    
    p <- ggplot(dat, aes(x = AGE, fill = ARM)) +
      geom_histogram(bins = 20, alpha = 0.7) +
      facet_wrap(~ARM) +
      labs(title = "Age Distribution by Treatment Arm",
           x = "Age (years)", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Sex distribution
  output$sex_plot <- renderPlotly({
    dat <- data()
    
    p <- ggplot(dat, aes(x = SEX, fill = ARM)) +
      geom_bar(position = "dodge", alpha = 0.7) +
      labs(title = "Sex Distribution by Treatment Arm",
           x = "Sex", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Stage distribution
  output$stage_plot <- renderPlotly({
    dat <- data()
    
    p <- ggplot(dat, aes(x = STAGE, fill = ARM)) +
      geom_bar(position = "dodge", alpha = 0.7) +
      labs(title = "Cancer Stage Distribution by Treatment Arm",
           x = "Stage", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ECOG distribution
  output$ecog_plot <- renderPlotly({
    dat <- data()
    
    p <- ggplot(dat, aes(x = factor(ECOG), fill = ARM)) +
      geom_bar(position = "dodge", alpha = 0.7) +
      labs(title = "ECOG Performance Status by Treatment Arm",
           x = "ECOG Score", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Forest plot
  output$forest_plot <- renderPlot({
    dat <- data()
    
    # Get user inputs
    variables_choice <- input$forest_variables
    theme_choice <- input$forest_theme
    alpha_level <- input$forest_alpha
    
    # Define variable groups
    all_vars <- c("ARM", "AGE", "SEX", "ECOG", "STAGE", "BIOMARKER")
    treatment_vars <- c("ARM")
    demo_vars <- c("AGE", "SEX")
    clinical_vars <- c("ECOG", "STAGE", "BIOMARKER")
    
    # Select variables based on user choice
    selected_vars <- switch(variables_choice,
                           "All Variables" = all_vars,
                           "Treatment Only" = treatment_vars,
                           "Demographics Only" = demo_vars,
                           "Clinical Only" = clinical_vars,
                           all_vars)
    
    # Create formula for Cox model
    formula_str <- paste("Surv(OS_TIME, OS_EVENT) ~", paste(selected_vars, collapse = " + "))
    cox_formula <- as.formula(formula_str)
    
    # Fit Cox model
    cox_model <- coxph(cox_formula, data = dat)
    
    # Extract coefficients and confidence intervals
    coef_summary <- summary(cox_model)
    coef_data <- data.frame(
      Variable = rownames(coef_summary$conf.int),
      HR = coef_summary$conf.int[, 1],
      Lower = coef_summary$conf.int[, 3],
      Upper = coef_summary$conf.int[, 4],
      p_value = coef_summary$coefficients[, 5]
    )
    
    # Clean up variable names for better readability
    coef_data$Variable <- gsub("ARMExperimental", "Treatment (Experimental vs Control)", coef_data$Variable)
    coef_data$Variable <- gsub("AGE", "Age (per year)", coef_data$Variable)
    coef_data$Variable <- gsub("SEXF", "Sex (Female vs Male)", coef_data$Variable)
    coef_data$Variable <- gsub("ECOG", "ECOG Score (per unit)", coef_data$Variable)
    coef_data$Variable <- gsub("STAGE", "Cancer Stage (per unit)", coef_data$Variable)
    coef_data$Variable <- gsub("BIOMARKER", "Biomarker Status", coef_data$Variable)
    
    # Add significance indicators based on user-defined alpha level
    coef_data$Significance <- ifelse(coef_data$p_value < alpha_level/10, "***",
                                    ifelse(coef_data$p_value < alpha_level/5, "**",
                                           ifelse(coef_data$p_value < alpha_level, "*", "")))
    
    # Format numbers for display
    coef_data$HR_formatted <- sprintf("%.3f", coef_data$HR)
    coef_data$CI_formatted <- sprintf("%.3f - %.3f", coef_data$Lower, coef_data$Upper)
    coef_data$p_formatted <- sprintf("%.4f", coef_data$p_value)
    
    # Create table text for forest plot
    tabletext <- list(
      list(coef_data$Variable),
      list(coef_data$HR_formatted),
      list(coef_data$CI_formatted),
      list(coef_data$p_formatted),
      list(coef_data$Significance)
    )
    
    # Define color schemes based on theme
    theme_colors <- switch(theme_choice,
                           "Default" = fpColors(box = "royalblue", lines = "darkblue", zero = "red"),
                           "Professional" = fpColors(box = "darkgreen", lines = "forestgreen", zero = "darkred"),
                           "Colorful" = fpColors(box = "purple", lines = "magenta", zero = "orange"),
                           "Minimal" = fpColors(box = "gray50", lines = "gray30", zero = "black"),
                           fpColors(box = "royalblue", lines = "darkblue", zero = "red"))
    
    # Create simplified forest plot
    forestplot(
      tabletext,
      coef_data$HR,
      coef_data$Lower,
      coef_data$Upper,
      zero = 1,
      cex = 1.0,
      boxsize = 0.4,
      col = theme_colors,
      xlab = "Hazard Ratio (95% CI)",
      title = paste("Cox Regression: Hazard Ratios for Overall Survival\n", 
                   "Variables:", variables_choice, "| Alpha:", alpha_level),
      xlim = c(0.1, 10),
      xticks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
      grid = TRUE
    )
  })
  
  # Dynamic forest plot table
  output$forest_table <- renderDT({
    dat <- data()
    
    # Get user inputs (same as forest plot)
    variables_choice <- input$forest_variables
    alpha_level <- input$forest_alpha
    
    # Define variable groups (same as forest plot)
    all_vars <- c("ARM", "AGE", "SEX", "ECOG", "STAGE", "BIOMARKER")
    treatment_vars <- c("ARM")
    demo_vars <- c("AGE", "SEX")
    clinical_vars <- c("ECOG", "STAGE", "BIOMARKER")
    
    # Select variables based on user choice
    selected_vars <- switch(variables_choice,
                           "All Variables" = all_vars,
                           "Treatment Only" = treatment_vars,
                           "Demographics Only" = demo_vars,
                           "Clinical Only" = clinical_vars,
                           all_vars)
    
    # Create formula for Cox model
    formula_str <- paste("Surv(OS_TIME, OS_EVENT) ~", paste(selected_vars, collapse = " + "))
    cox_formula <- as.formula(formula_str)
    
    # Fit Cox model
    cox_model <- coxph(cox_formula, data = dat)
    
    # Extract coefficients and confidence intervals
    coef_summary <- summary(cox_model)
    coef_data <- data.frame(
      Variable = rownames(coef_summary$conf.int),
      HR = coef_summary$conf.int[, 1],
      Lower_CI = coef_summary$conf.int[, 3],
      Upper_CI = coef_summary$conf.int[, 4],
      p_value = coef_summary$coefficients[, 5]
    )
    
    # Clean up variable names
    coef_data$Variable <- gsub("ARMExperimental", "Treatment (Experimental vs Control)", coef_data$Variable)
    coef_data$Variable <- gsub("AGE", "Age (per year)", coef_data$Variable)
    coef_data$Variable <- gsub("SEXF", "Sex (Female vs Male)", coef_data$Variable)
    coef_data$Variable <- gsub("ECOG", "ECOG Score (per unit)", coef_data$Variable)
    coef_data$Variable <- gsub("STAGE", "Cancer Stage (per unit)", coef_data$Variable)
    coef_data$Variable <- gsub("BIOMARKER", "Biomarker Status", coef_data$Variable)
    
    # Format numbers
    coef_data$HR_formatted <- sprintf("%.3f", coef_data$HR)
    coef_data$CI_formatted <- sprintf("%.3f - %.3f", coef_data$Lower_CI, coef_data$Upper_CI)
    coef_data$p_formatted <- sprintf("%.4f", coef_data$p_value)
    
    # Add significance and interpretation based on user-defined alpha level
    coef_data$Significance <- ifelse(coef_data$p_value < alpha_level/10, "***",
                                    ifelse(coef_data$p_value < alpha_level/5, "**",
                                           ifelse(coef_data$p_value < alpha_level, "*", "")))
    
    coef_data$Interpretation <- ifelse(coef_data$HR < 1, 
                                      paste0("Reduces risk by ", round((1-coef_data$HR)*100, 1), "%"),
                                      paste0("Increases risk by ", round((coef_data$HR-1)*100, 1), "%"))
    
    # Create display table
    display_table <- data.frame(
      Variable = coef_data$Variable,
      "Hazard Ratio" = coef_data$HR_formatted,
      "95% CI" = coef_data$CI_formatted,
      "P-value" = coef_data$p_formatted,
      "Significance" = coef_data$Significance,
      "Interpretation" = coef_data$Interpretation
    )
    
    datatable(display_table,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              caption = "Cox Proportional Hazards Model Results for Overall Survival") %>%
      formatStyle(
        'P.value',
        color = styleInterval(c(0.001, 0.01, 0.05), c('red', 'orange', 'blue', 'black'))
      ) %>%
      formatStyle(
        'Significance',
        fontWeight = 'bold',
        color = styleEqual(c('***', '**', '*', ''), c('red', 'orange', 'blue', 'black'))
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)