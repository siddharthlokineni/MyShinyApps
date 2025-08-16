# Enhanced Colorful and Informative Sankey App
# Save this as app.R

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)  # Added for unite function

# Generate more detailed and realistic data
generate_enhanced_data <- function(n = 200, seed = 123) {
  set.seed(seed)
  
  data.frame(
    patient_id = 1:n,
    age_group = sample(c("Age < 40", "Age 40-64", "Age 65+"), n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    treatment = sample(c("Drug A", "Drug B", "Placebo"), n, replace = TRUE),
    ae_severity = sample(c("No AE", "Mild AE", "Moderate AE", "Severe AE"), n, replace = TRUE, prob = c(0.4, 0.35, 0.2, 0.05)),
    response = sample(c("Complete Response", "Partial Response", "No Response"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    final_outcome = sample(c("Study Completed", "Discontinued - AE", "Discontinued - Other", "Death"), n, replace = TRUE, prob = c(0.75, 0.12, 0.11, 0.02)),
    stringsAsFactors = FALSE
  )
}

# Create comprehensive Sankey data with multiple pathways
create_enhanced_sankey <- function(data) {
  
  # 1. Demographics to Treatment
  demo_treat <- data %>%
    unite("demo", age_group, gender, sep = " - ") %>%
    count(demo, treatment) %>%
    rename(source = demo, target = treatment, value = n)
  
  # 2. Treatment to AE Severity
  treat_ae <- data %>%
    count(treatment, ae_severity) %>%
    rename(source = treatment, target = ae_severity, value = n)
  
  # 3. AE Severity to Treatment Response
  ae_response <- data %>%
    count(ae_severity, response) %>%
    rename(source = ae_severity, target = response, value = n)
  
  # 4. Treatment Response to Final Outcome
  response_outcome <- data %>%
    count(response, final_outcome) %>%
    rename(source = response, target = final_outcome, value = n)
  
  # Combine all flows
  flows <- bind_rows(demo_treat, treat_ae, ae_response, response_outcome) %>%
    filter(value > 0)
  
  # Create comprehensive node list
  all_nodes <- unique(c(flows$source, flows$target))
  
  # Enhanced color scheme - much more colorful
  get_node_color <- function(node) {
    case_when(
      # Demographics - Cool tones
      grepl("Age < 40.*Male", node) ~ "#3498DB",      # Blue
      grepl("Age < 40.*Female", node) ~ "#5DADE2",    # Light Blue
      grepl("Age 40-64.*Male", node) ~ "#E91E63",     # Pink
      grepl("Age 40-64.*Female", node) ~ "#F48FB1",   # Light Pink
      grepl("Age 65+.*Male", node) ~ "#9C27B0",       # Purple
      grepl("Age 65+.*Female", node) ~ "#CE93D8",     # Light Purple
      
      # Treatments - Vibrant colors
      node == "Drug A" ~ "#FF5722",                   # Deep Orange
      node == "Drug B" ~ "#4CAF50",                   # Green
      node == "Placebo" ~ "#FFC107",                  # Amber
      
      # Adverse Events - Warning colors
      node == "No AE" ~ "#8BC34A",                    # Light Green
      node == "Mild AE" ~ "#FFEB3B",                  # Yellow
      node == "Moderate AE" ~ "#FF9800",              # Orange
      node == "Severe AE" ~ "#F44336",                # Red
      
      # Treatment Response - Success gradient
      node == "Complete Response" ~ "#4CAF50",         # Green
      node == "Partial Response" ~ "#CDDC39",          # Lime
      node == "No Response" ~ "#795548",               # Brown
      
      # Final Outcomes - Meaningful colors
      node == "Study Completed" ~ "#2E7D32",           # Dark Green
      node == "Discontinued - AE" ~ "#D32F2F",         # Dark Red
      node == "Discontinued - Other" ~ "#F57C00",      # Dark Orange
      node == "Death" ~ "#424242",                     # Dark Gray
      
      TRUE ~ "#9E9E9E"                                 # Default Gray
    )
  }
  
  # Create nodes dataframe with colors
  nodes <- data.frame(
    id = 0:(length(all_nodes)-1),
    label = all_nodes,
    color = sapply(all_nodes, get_node_color),
    stringsAsFactors = FALSE
  )
  
  # Create links with node IDs and colors
  links <- flows %>%
    left_join(nodes, by = c("source" = "label")) %>%
    rename(source_id = id, source_color = color) %>%
    left_join(nodes, by = c("target" = "label")) %>%
    rename(target_id = id, target_color = color) %>%
    mutate(
      # Color links based on source node with transparency
      link_color = mapply(function(col) {
        rgb <- col2rgb(col)
        sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], 0.5)
      }, source_color)
      # Add transparency
    ) %>%
    select(source = source_id, target = target_id, value, link_color)
  
  return(list(nodes = nodes, links = links))
}

# Calculate summary statistics
calculate_stats <- function(data) {
  list(
    total_patients = nrow(data),
    completion_rate = round(mean(data$final_outcome == "Study Completed") * 100, 1),
    ae_rate = round(mean(data$ae_severity != "No AE") * 100, 1),
    response_rate = round(mean(data$response %in% c("Complete Response", "Partial Response")) * 100, 1),
    avg_age_completed = round(mean(data$age_group[data$final_outcome == "Study Completed"] == "Age 65+") * 100, 1)
  )
}

# Enhanced UI with more information
ui <- fluidPage(
  tags$head(
    tags$title("Enhanced Clinical Trial Sankey"),
    tags$style(HTML("
      .info-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 15px;
        border-radius: 10px;
        margin-bottom: 15px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .stat-box {
        background: white;
        border: 2px solid #e3f2fd;
        border-radius: 8px;
        padding: 10px;
        margin: 5px 0;
        text-align: center;
      }
      .stat-number {
        font-size: 24px;
        font-weight: bold;
        color: #1976d2;
      }
      .stat-label {
        font-size: 12px;
        color: #666;
      }
      body {
        background-color: #f8f9fa;
      }
      .well {
        background: white;
        border: none;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
    "))
  ),
  
  div(class = "info-box",
      h2("Enhanced Clinical Trial Patient Journey Analysis", style = "margin: 0; text-align: center;"),
      p("Comprehensive visualization of patient flow through demographics, treatments, adverse events, responses, and outcomes", 
        style = "margin: 5px 0 0 0; text-align: center; opacity: 0.9;")
  ), 
  
  fluidRow(
    column(3,
           wellPanel(
             h4("Controls", style = "color: #1976d2; margin-top: 0;"),
             numericInput("n_patients", "Number of Patients:", 
                          value = 200, min = 100, max = 400, step = 25),
             numericInput("seed_val", "Random Seed:",
                          value = 123, min = 1, max = 9999),
             actionButton("refresh", "Generate New Data", 
                          class = "btn-primary btn-block",
                          style = "background: linear-gradient(45deg, #667eea, #764ba2); border: none;"),
             br(),
             
             h5("Color Legend", style = "color: #1976d2; margin-top: 15px;"),
             div(style = "font-size: 11px;",
                 p("Age < 40: Blue tones", style = "margin: 2px 0;"),
                 p("Age 40-64: Pink tones", style = "margin: 2px 0;"),
                 p("Age 65+: Purple tones", style = "margin: 2px 0;"),
                 p("Drug A: Orange", style = "margin: 2px 0;"),
                 p("Drug B: Green", style = "margin: 2px 0;"),
                 p("Placebo: Yellow", style = "margin: 2px 0;")
             ),
             
             h5("Study Statistics", style = "color: #1976d2; margin-top: 15px;"),
             uiOutput("stats_display")
           )
    ),
    
    column(9,
           plotlyOutput("sankey_plot", height = "700px"),
           
           br(),
           
           fluidRow(
             column(6,
                    div(class = "well",
                        h5("Patient Demographics", style = "color: #1976d2;"),
                        tableOutput("demo_table")
                    )
             ),
             column(6,
                    div(class = "well",
                        h5("Treatment Outcomes", style = "color: #1976d2;"),
                        tableOutput("outcome_table")
                    )
             )
           )
    )
  )
)

# Enhanced server with more functionality
server <- function(input, output, session) {
  
  # Reactive data
  patient_data <- reactive({
    input$refresh
    generate_enhanced_data(input$n_patients, input$seed_val)
  })
  
  # Reactive Sankey data
  plot_data <- reactive({
    create_enhanced_sankey(patient_data())
  })
  
  # Main Sankey plot
  output$sankey_plot <- renderPlotly({
    
    sankey_data <- plot_data()
    
    plot_ly(
      type = "sankey",
      orientation = "h",
      
      node = list(
        label = sankey_data$nodes$label,
        color = sankey_data$nodes$color,
        pad = 15,
        thickness = 25,
        line = list(color = "white", width = 2),
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          "Total Flow: %{value}<br>",
          "<extra></extra>"
        )
      ),
      
      link = list(
        source = sankey_data$links$source,
        target = sankey_data$links$target,
        value = sankey_data$links$value,
        color = sankey_data$links$link_color,
        hovertemplate = paste0(
          "<b>Patient Flow</b><br>",
          "Count: %{value} patients<br>",
          "<extra></extra>"
        )
      )
    ) %>%
      layout(
        title = list(
          text = "Multi-Stage Patient Journey with Treatment Responses",
          font = list(size = 16, color = "#1976d2")
        ),
        font = list(size = 11),
        margin = list(l = 50, r = 50, t = 60, b = 20),
        plot_bgcolor = "rgba(248,249,250,1)",
        paper_bgcolor = "rgba(248,249,250,1)"
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"),
        displaylogo = FALSE
      )
  })
  
  # Statistics display
  output$stats_display <- renderUI({
    stats <- calculate_stats(patient_data())
    
    tagList(
      div(class = "stat-box",
          div(class = "stat-number", stats$total_patients),
          div(class = "stat-label", "Total Patients")
      ),
      div(class = "stat-box",
          div(class = "stat-number", paste0(stats$completion_rate, "%")),
          div(class = "stat-label", "Completion Rate")
      ),
      div(class = "stat-box",
          div(class = "stat-number", paste0(stats$ae_rate, "%")),
          div(class = "stat-label", "Adverse Event Rate")
      ),
      div(class = "stat-box",
          div(class = "stat-number", paste0(stats$response_rate, "%")),
          div(class = "stat-label", "Response Rate")
      )
    )
  })
  
  # Demographics table
  output$demo_table <- renderTable({
    patient_data() %>%
      count(age_group, gender) %>%
      arrange(age_group, gender) %>%
      rename("Age Group" = age_group, "Gender" = gender, "Count" = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Outcomes table
  output$outcome_table <- renderTable({
    patient_data() %>%
      count(treatment, final_outcome) %>%
      arrange(treatment, final_outcome) %>%
      rename("Treatment" = treatment, "Outcome" = final_outcome, "Count" = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Run the app
shinyApp(ui = ui, server = server)