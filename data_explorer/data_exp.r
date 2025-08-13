library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(randomizr)

# Simulate CDISC-like data structure
generate_sample_data <- function() {
  set.seed(123)
  n <- 200
  
  # Demographics (DM domain)
  dm_data <- data.frame(
    USUBJID = paste0("SUBJ-", sprintf("%03d", 1:n)),
    STUDYID = "STUDY-001",
    ARM = sample(c("Placebo", "Treatment A", "Treatment B"), n, replace = TRUE),
    AGE = round(rnorm(n, 55, 12)),
    SEX = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    RACE = sample(c("WHITE", "BLACK", "ASIAN", "OTHER"), n, replace = TRUE, prob = c(0.7, 0.15, 0.1, 0.05)),
    COUNTRY = sample(c("USA", "Canada", "Germany", "Japan"), n, replace = TRUE)
  )
  
  # Vital Signs (VS domain)
  vs_data <- data.frame()
  for(subj in dm_data$USUBJID) {
    visits <- c("Baseline", "Week 4", "Week 8", "Week 12", "Week 24")
    for(visit in visits) {
      baseline_sbp <- rnorm(1, 140, 20)
      treatment_effect <- ifelse(dm_data$ARM[dm_data$USUBJID == subj] == "Treatment A", -15, 
                                ifelse(dm_data$ARM[dm_data$USUBJID == subj] == "Treatment B", -25, -2))
      visit_effect <- case_when(
        visit == "Baseline" ~ 0,
        visit == "Week 4" ~ treatment_effect * 0.3,
        visit == "Week 8" ~ treatment_effect * 0.6,
        visit == "Week 12" ~ treatment_effect * 0.8,
        visit == "Week 24" ~ treatment_effect * 1.0
      )
      
      vs_data <- rbind(vs_data, data.frame(
        USUBJID = subj,
        VISIT = visit,
        VSTESTCD = "SYSBP",
        VSORRES = round(baseline_sbp + visit_effect + rnorm(1, 0, 5)),
        VSORRESU = "mmHg"
      ))
    }
  }
  
  # Pharmacokinetics (PC domain)
  pk_data <- data.frame()
  for(subj in sample(dm_data$USUBJID[dm_data$ARM != "Placebo"], 100)) {
    timepoints <- c(0, 0.5, 1, 2, 4, 6, 8, 12, 24)
    dose <- ifelse(dm_data$ARM[dm_data$USUBJID == subj] == "Treatment A", 100, 200)
    
    for(time in timepoints) {
      # Simple one-compartment PK model
      if(time == 0) {
        conc <- 0
      } else {
        ka <- 1.5 + rnorm(1, 0, 0.2)
        ke <- 0.1 + rnorm(1, 0, 0.02)
        conc <- (dose * ka / (ka - ke)) * (exp(-ke * time) - exp(-ka * time)) * exp(rnorm(1, 0, 0.1))
      }
      
      pk_data <- rbind(pk_data, data.frame(
        USUBJID = subj,
        PCDY = 1,
        PCTPTNUM = time,
        PCTEST = "Drug Concentration",
        PCORRES = round(conc, 2),
        PCORRESU = "ng/mL"
      ))
    }
  }
  
  return(list(dm = dm_data, vs = vs_data, pk = pk_data))
}

# Generate the data
clinical_data <- generate_sample_data()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Clinical Trial Data Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Efficacy Analysis", tabName = "efficacy", icon = icon("chart-line")),
      menuItem("PK/PD Analysis", tabName = "pkpd", icon = icon("flask")),
      menuItem("Safety Overview", tabName = "safety", icon = icon("shield-alt")),
      menuItem("Data Tables", tabName = "tables", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # Demographics Tab
      tabItem(tabName = "demographics",
        fluidRow(
          box(width = 6, title = "Age Distribution by Treatment Arm", status = "primary", solidHeader = TRUE,
              plotlyOutput("age_plot")
          ),
          box(width = 6, title = "Gender Distribution", status = "primary", solidHeader = TRUE,
              plotlyOutput("gender_plot")
          )
        ),
        fluidRow(
          box(width = 12, title = "Demographics Summary Table", status = "primary", solidHeader = TRUE,
              DT::dataTableOutput("demo_table")
          )
        )
      ),
      
      # Efficacy Tab
      tabItem(tabName = "efficacy",
        fluidRow(
          box(width = 4, title = "Analysis Controls", status = "primary", solidHeader = TRUE,
              selectInput("efficacy_endpoint", "Primary Endpoint:", 
                         choices = c("Systolic BP" = "SYSBP")),
              selectInput("efficacy_visits", "Visits to Include:", 
                         choices = c("Baseline", "Week 4", "Week 8", "Week 12", "Week 24"),
                         selected = c("Baseline", "Week 12", "Week 24"), multiple = TRUE),
              checkboxInput("show_individual", "Show Individual Profiles", FALSE)
          ),
          box(width = 8, title = "Efficacy Over Time", status = "primary", solidHeader = TRUE,
              plotlyOutput("efficacy_plot", height = "400px")
          )
        ),
        fluidRow(
          box(width = 12, title = "Statistical Summary", status = "primary", solidHeader = TRUE,
              verbatimTextOutput("efficacy_stats")
          )
        )
      ),
      
      # PK/PD Tab
      tabItem(tabName = "pkpd",
        fluidRow(
          box(width = 4, title = "PK Analysis Options", status = "primary", solidHeader = TRUE,
              radioButtons("pk_display", "Display Type:",
                          choices = list("Mean Profile" = "mean", 
                                       "Individual Profiles" = "individual",
                                       "AUC Analysis" = "auc")),
              conditionalPanel(condition = "input.pk_display == 'individual'",
                              numericInput("n_subjects", "Number of Subjects to Show:", 
                                         value = 10, min = 1, max = 50)
              )
          ),
          box(width = 8, title = "Pharmacokinetic Profiles", status = "primary", solidHeader = TRUE,
              plotlyOutput("pk_plot", height = "400px")
          )
        ),
        fluidRow(
          box(width = 6, title = "PK Parameters Summary", status = "primary", solidHeader = TRUE,
              DT::dataTableOutput("pk_params_table")
          ),
          box(width = 6, title = "Dose Proportionality", status = "primary", solidHeader = TRUE,
              plotlyOutput("dose_prop_plot")
          )
        )
      ),
      
      # Safety Tab
      tabItem(tabName = "safety",
        fluidRow(
          box(width = 6, title = "Treatment-Emergent Adverse Events", status = "warning", solidHeader = TRUE,
              plotlyOutput("safety_overview")
          ),
          box(width = 6, title = "Laboratory Abnormalities", status = "warning", solidHeader = TRUE,
              plotlyOutput("lab_abnormalities")
          )
        ),
        fluidRow(
          box(width = 12, title = "Safety Summary Table", status = "warning", solidHeader = TRUE,
              DT::dataTableOutput("safety_table")
          )
        )
      ),
      
      # Data Tables Tab
      tabItem(tabName = "tables",
        fluidRow(
          box(width = 12, title = "CDISC Data Domains", status = "info", solidHeader = TRUE,
              tabsetPanel(
                tabPanel("Demographics (DM)", DT::dataTableOutput("dm_table")),
                tabPanel("Vital Signs (VS)", DT::dataTableOutput("vs_table")),
                tabPanel("Pharmacokinetics (PC)", DT::dataTableOutput("pk_table"))
              )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Demographics plots
  output$age_plot <- renderPlotly({
    p <- ggplot(clinical_data$dm, aes(x = ARM, y = AGE, fill = ARM)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      theme_minimal() +
      labs(title = "Age Distribution by Treatment Arm",
           x = "Treatment Arm", y = "Age (years)") +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$gender_plot <- renderPlotly({
    gender_summary <- clinical_data$dm %>%
      group_by(ARM, SEX) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(ARM) %>%
      mutate(percentage = round(count/sum(count) * 100, 1))
    
    p <- ggplot(gender_summary, aes(x = ARM, y = percentage, fill = SEX)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Gender Distribution by Treatment Arm",
           x = "Treatment Arm", y = "Percentage (%)", fill = "Gender")
    ggplotly(p)
  })
  
  output$demo_table <- DT::renderDataTable({
    demo_summary <- clinical_data$dm %>%
      group_by(ARM) %>%
      summarise(
        N = n(),
        `Mean Age (SD)` = paste0(round(mean(AGE), 1), " (", round(sd(AGE), 1), ")"),
        `Male n (%)` = paste0(sum(SEX == "M"), " (", round(sum(SEX == "M")/n()*100, 1), ")"),
        `Female n (%)` = paste0(sum(SEX == "F"), " (", round(sum(SEX == "F")/n()*100, 1), ")"),
        .groups = 'drop'
      )
    
    DT::datatable(demo_summary, options = list(dom = 't', pageLength = 10))
  })
  
  # Efficacy analysis
  output$efficacy_plot <- renderPlotly({
    filtered_vs <- clinical_data$vs %>%
      filter(VSTESTCD == input$efficacy_endpoint,
             VISIT %in% input$efficacy_visits) %>%
      left_join(clinical_data$dm[, c("USUBJID", "ARM")], by = "USUBJID")
    
    if(input$show_individual) {
      p <- ggplot(filtered_vs, aes(x = VISIT, y = VSORRES, color = ARM, group = USUBJID)) +
        geom_line(alpha = 0.3) +
        geom_point(alpha = 0.5) +
        stat_summary(aes(group = ARM), fun = mean, geom = "line", size = 2, alpha = 0.8) +
        theme_minimal() +
        labs(title = "Individual and Mean Profiles",
             x = "Visit", y = "Systolic BP (mmHg)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      summary_data <- filtered_vs %>%
        group_by(ARM, VISIT) %>%
        summarise(
          mean_val = mean(VSORRES, na.rm = TRUE),
          se_val = sd(VSORRES, na.rm = TRUE) / sqrt(n()),
          .groups = 'drop'
        )
      
      p <- ggplot(summary_data, aes(x = VISIT, y = mean_val, color = ARM, group = ARM)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = mean_val - se_val, ymax = mean_val + se_val), 
                     width = 0.2, size = 1) +
        theme_minimal() +
        labs(title = "Mean Change Over Time with Standard Error",
             x = "Visit", y = "Mean Systolic BP (mmHg)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p)
  })
  
  output$efficacy_stats <- renderText({
    filtered_vs <- clinical_data$vs %>%
      filter(VSTESTCD == input$efficacy_endpoint,
             VISIT %in% c("Baseline", "Week 24")) %>%
      left_join(clinical_data$dm[, c("USUBJID", "ARM")], by = "USUBJID") %>%
      select(USUBJID, ARM, VISIT, VSORRES) %>%
      tidyr::pivot_wider(names_from = VISIT, values_from = VSORRES) %>%
      mutate(Change = `Week 24` - Baseline) %>%
      filter(!is.na(Change))
    
    if(nrow(filtered_vs) > 0) {
      stats_summary <- filtered_vs %>%
        group_by(ARM) %>%
        summarise(
          N = n(),
          `Mean Change` = round(mean(Change, na.rm = TRUE), 2),
          `SD Change` = round(sd(Change, na.rm = TRUE), 2),
          `SE Change` = round(sd(Change, na.rm = TRUE) / sqrt(n()), 2),
          .groups = 'drop'
        )
      
      result <- "CHANGE FROM BASELINE TO WEEK 24 SUMMARY:\n\n"
      for(i in 1:nrow(stats_summary)) {
        result <- paste0(result, 
                        stats_summary$ARM[i], ":\n",
                        "  N = ", stats_summary$N[i], "\n",
                        "  Mean Change = ", stats_summary$`Mean Change`[i], " mmHg\n",
                        "  SD = ", stats_summary$`SD Change`[i], "\n",
                        "  SE = ", stats_summary$`SE Change`[i], "\n\n")
      }
      
      # Simple ANOVA
      if(length(unique(filtered_vs$ARM)) > 1) {
        anova_result <- aov(Change ~ ARM, data = filtered_vs)
        p_value <- round(summary(anova_result)[[1]][["Pr(>F)"]][1], 4)
        result <- paste0(result, "ANOVA p-value for treatment difference: ", p_value)
      }
      
      return(result)
    } else {
      return("No data available for selected visits.")
    }
  })
  
  # PK Analysis
  output$pk_plot <- renderPlotly({
    if(input$pk_display == "mean") {
      pk_summary <- clinical_data$pk %>%
        left_join(clinical_data$dm[, c("USUBJID", "ARM")], by = "USUBJID") %>%
        group_by(ARM, PCTPTNUM) %>%
        summarise(
          mean_conc = mean(PCORRES, na.rm = TRUE),
          se_conc = sd(PCORRES, na.rm = TRUE) / sqrt(n()),
          .groups = 'drop'
        )
      
      p <- ggplot(pk_summary, aes(x = PCTPTNUM, y = mean_conc, color = ARM)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = mean_conc - se_conc, ymax = mean_conc + se_conc), 
                     width = 0.5) +
        scale_y_log10() +
        theme_minimal() +
        labs(title = "Mean PK Profiles",
             x = "Time (hours)", y = "Concentration (ng/mL, log scale)")
      
    } else if(input$pk_display == "individual") {
      pk_individual <- clinical_data$pk %>%
        left_join(clinical_data$dm[, c("USUBJID", "ARM")], by = "USUBJID") %>%
        filter(USUBJID %in% sample(unique(USUBJID), min(input$n_subjects, 50)))
      
      p <- ggplot(pk_individual, aes(x = PCTPTNUM, y = PCORRES + 0.1, 
                                   color = ARM, group = USUBJID)) +
        geom_line(alpha = 0.6) +
        geom_point(alpha = 0.6) +
        scale_y_log10() +
        theme_minimal() +
        labs(title = paste("Individual PK Profiles (n =", input$n_subjects, ")"),
             x = "Time (hours)", y = "Concentration (ng/mL, log scale)")
      
    } else { # AUC analysis
      # Calculate AUC using trapezoidal rule
      auc_data <- clinical_data$pk %>%
        left_join(clinical_data$dm[, c("USUBJID", "ARM")], by = "USUBJID") %>%
        group_by(USUBJID, ARM) %>%
        arrange(PCTPTNUM) %>%
        summarise(
          AUC_0_24 = sum(diff(PCTPTNUM) * (head(PCORRES, -1) + tail(PCORRES, -1)) / 2, na.rm = TRUE),
          Cmax = max(PCORRES, na.rm = TRUE),
          Tmax = PCTPTNUM[which.max(PCORRES)],
          .groups = 'drop'
        )
      
      p <- ggplot(auc_data, aes(x = ARM, y = AUC_0_24, fill = ARM)) +
        geom_boxplot(alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.6) +
        theme_minimal() +
        labs(title = "AUC₀₋₂₄ by Treatment Arm",
             x = "Treatment Arm", y = "AUC₀₋₂₄ (ng*h/mL)") +
        theme(legend.position = "none")
    }
    
    ggplotly(p)
  })
  
  # PK Parameters Table
  output$pk_params_table <- DT::renderDataTable({
    auc_data <- clinical_data$pk %>%
      left_join(clinical_data$dm[, c("USUBJID", "ARM")], by = "USUBJID") %>%
      group_by(USUBJID, ARM) %>%
      arrange(PCTPTNUM) %>%
      summarise(
        AUC_0_24 = sum(diff(PCTPTNUM) * (head(PCORRES, -1) + tail(PCORRES, -1)) / 2, na.rm = TRUE),
        Cmax = max(PCORRES, na.rm = TRUE),
        Tmax = PCTPTNUM[which.max(PCORRES)],
        .groups = 'drop'
      ) %>%
      group_by(ARM) %>%
      summarise(
        N = n(),
        `AUC₀₋₂₄ Mean (CV%)` = paste0(round(mean(AUC_0_24), 1), " (", 
                                     round(sd(AUC_0_24)/mean(AUC_0_24)*100, 1), ")"),
        `Cmax Mean (CV%)` = paste0(round(mean(Cmax), 1), " (", 
                                  round(sd(Cmax)/mean(Cmax)*100, 1), ")"),
        `Tmax Median (Range)` = paste0(median(Tmax), " (", min(Tmax), "-", max(Tmax), ")"),
        .groups = 'drop'
      )
    
    DT::datatable(auc_data, options = list(dom = 't', pageLength = 10))
  })
  
  # Dose Proportionality
  output$dose_prop_plot <- renderPlotly({
    dose_prop <- clinical_data$pk %>%
      left_join(clinical_data$dm[, c("USUBJID", "ARM")], by = "USUBJID") %>%
      group_by(USUBJID, ARM) %>%
      summarise(
        AUC = sum(diff(PCTPTNUM) * (head(PCORRES, -1) + tail(PCORRES, -1)) / 2, na.rm = TRUE),
        Cmax = max(PCORRES, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(Dose = ifelse(ARM == "Treatment A", 100, 200)) %>%
      filter(ARM != "Placebo")
    
    p <- ggplot(dose_prop, aes(x = Dose, y = AUC, color = ARM)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(title = "Dose-AUC Relationship",
           x = "Dose (mg)", y = "AUC₀₋₂₄ (ng*h/mL)")
    
    ggplotly(p)
  })
  
  # Safety plots (simulated data)
  output$safety_overview <- renderPlotly({
    # Simulate AE data
    ae_data <- data.frame(
      ARM = rep(c("Placebo", "Treatment A", "Treatment B"), each = 3),
      AE_Type = rep(c("Headache", "Nausea", "Dizziness"), 3),
      Incidence = c(15, 12, 8, 22, 18, 15, 25, 21, 18)
    )
    
    p <- ggplot(ae_data, aes(x = AE_Type, y = Incidence, fill = ARM)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Common Adverse Events by Treatment",
           x = "Adverse Event", y = "Incidence (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$lab_abnormalities <- renderPlotly({
    # Simulate lab abnormalities
    lab_data <- data.frame(
      ARM = rep(c("Placebo", "Treatment A", "Treatment B"), each = 4),
      Lab_Test = rep(c("ALT >3x ULN", "AST >3x ULN", "Bilirubin >2x ULN", "Creatinine >1.5x ULN"), 3),
      Percentage = c(2, 1, 1, 3, 5, 4, 2, 4, 8, 6, 3, 5)
    )
    
    p <- ggplot(lab_data, aes(x = Lab_Test, y = Percentage, fill = ARM)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Laboratory Abnormalities",
           x = "Laboratory Test", y = "Percentage (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Data tables
  output$dm_table <- DT::renderDataTable({
    DT::datatable(clinical_data$dm, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$vs_table <- DT::renderDataTable({
    DT::datatable(clinical_data$vs, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$pk_table <- DT::renderDataTable({
    DT::datatable(clinical_data$pk, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$safety_table <- DT::renderDataTable({
    safety_summary <- data.frame(
      `Treatment Arm` = c("Placebo", "Treatment A", "Treatment B"),
      `N` = c(67, 66, 67),
      `Any AE n (%)` = c("45 (67.2)", "52 (78.8)", "58 (86.6)"),
      `Serious AE n (%)` = c("3 (4.5)", "5 (7.6)", "7 (10.4)"),
      `Discontinuation due to AE n (%)` = c("2 (3.0)", "4 (6.1)", "6 (9.0)"),
      check.names = FALSE
    )
    
    DT::datatable(safety_summary, options = list(dom = 't', pageLength = 10))
  })
}

# Run the application
shinyApp(ui = ui, server = server)