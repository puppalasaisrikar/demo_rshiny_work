library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(glmnet)
library(DT)
library(shinyBS)
library(digest)
library(randomForest)
library(e1071)
library(rpart)
library(kernlab)

## ============================================================================
## PORTFOLIO DEMONSTRATION APP
## Healthcare Screening Intervention Analysis Tool
## 
## This application demonstrates advanced R Shiny development skills including:
## - Complex reactive programming with multiple analysis modes
## - Dynamic UI generation based on user inputs
## - Machine learning model integration (6 different algorithms)
## - Interactive data tables and visualizations
## - Multi-population comparative analysis
## - Comprehensive parameter validation
## - Advanced state management
## ============================================================================

## ---------------------------
## Data Preparation Functions
## ---------------------------
data_prep <- function(file_path, outcome_name) {
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  df <- read_csv(file_path, col_names = FALSE, skip = 1)
  input_cols <- c("SCREEN1_before", "SCREEN1_during", "SCREEN1_after",
                  "SCREEN2_before", "SCREEN2_during", "SCREEN2_after",
                  "DIAG_before", "DIAG_during", "DIAG_after")
  person_cols <- paste0("P_", 1:180)
  colnames(df) <- c(input_cols, person_cols)
  df$row_id <- seq_len(nrow(df))
  df_long <- pivot_longer(df, cols = all_of(person_cols),
                          names_to = "Person", values_to = outcome_name)
  return(df_long)
}

# Create demographic mapping
create_mapping_df <- function() {
  genders <- c("male", "female")
  categories <- c("cat_a", "cat_b", "cat_c")
  age_groups <- c("45-49", "50-54", "55-59", "60-64", "65-69", "70-74")
  
  combinations <- expand.grid(
    Gender = genders,
    Category = categories,
    AgeGroup = age_groups,
    stringsAsFactors = FALSE
  )
  
  combinations$P_values <- rep(1:180, length.out = nrow(combinations))
  return(combinations)
}

get_age_group <- function(age) {
  cut(age,
      breaks = c(44, 49, 54, 59, 64, 69, 74),
      labels = c("45-49", "50-54", "55-59", "60-64", "65-69", "70-74"),
      right = TRUE)
}

## ---------------------------
## Parameter Validation Functions
## ---------------------------
validate_screening_parameters <- function(screen1_before, screen1_during, screen1_after,
                                          screen2_before, screen2_during, screen2_after,
                                          diag_before, diag_during, diag_after) {
  errors <- c()
  
  # Screening method 1 validation
  if (screen1_before >= screen1_during) {
    errors <- c(errors, "Screening Method 1 'Before' must be less than 'During'")
  }
  if (screen1_after >= screen1_during) {
    errors <- c(errors, "Screening Method 1 'After' must be less than 'During'")
  }
  if (screen1_before >= screen1_after) {
    errors <- c(errors, "Screening Method 1 'Before' must be less than 'After'")
  }
  
  # Screening method 2 validation
  if (screen2_before >= screen2_during) {
    errors <- c(errors, "Screening Method 2 'Before' must be less than 'During'")
  }
  if (screen2_after >= screen2_during) {
    errors <- c(errors, "Screening Method 2 'After' must be less than 'During'")
  }
  if (screen2_before >= screen2_after) {
    errors <- c(errors, "Screening Method 2 'Before' must be less than 'After'")
  }
  
  # Diagnostic validation
  if (diag_before >= diag_during) {
    errors <- c(errors, "Diagnostic 'Before' must be less than 'During'")
  }
  if (diag_after >= diag_during) {
    errors <- c(errors, "Diagnostic 'After' must be less than 'During'")
  }
  if (diag_before >= diag_after) {
    errors <- c(errors, "Diagnostic 'Before' must be less than 'After'")
  }
  
  return(errors)
}

## ---------------------------
## Sub-Population Validation Functions
## ---------------------------
validate_subpopulations <- function(subpop_data, input_mode, total_population) {
  errors <- c()
  
  if (nrow(subpop_data) == 0) {
    return(list(valid = FALSE, errors = "No sub-populations defined"))
  }
  
  empty_names <- which(is.na(subpop_data$name) | subpop_data$name == "")
  if (length(empty_names) > 0) {
    errors <- c(errors, paste("Sub-population", empty_names, "must have a name"))
  }
  
  duplicate_names <- duplicated(subpop_data$name[!is.na(subpop_data$name)])
  if (any(duplicate_names)) {
    errors <- c(errors, "Sub-population names must be unique")
  }
  
  if (input_mode == "percent") {
    total_percent <- sum(subpop_data$value, na.rm = TRUE)
    if (abs(total_percent - 100) > 0.01) {
      errors <- c(errors, paste("Total percentages must equal 100%. Current total:", round(total_percent, 2), "%"))
    }
    
    invalid_percent <- which(subpop_data$value <= 0 | subpop_data$value > 100)
    if (length(invalid_percent) > 0) {
      errors <- c(errors, paste("Sub-population", invalid_percent, "percentage must be between 0.1 and 100"))
    }
  } else {
    total_absolute <- sum(subpop_data$value, na.rm = TRUE)
    if (total_absolute != total_population) {
      errors <- c(errors, paste("Total sub-population counts must equal total population.",
                                "Current total:", format(total_absolute, big.mark = ","),
                                "Expected:", format(total_population, big.mark = ",")))
    }
    
    invalid_absolute <- which(subpop_data$value <= 0 | subpop_data$value > total_population)
    if (length(invalid_absolute) > 0) {
      errors <- c(errors, paste("Sub-population", invalid_absolute, "count must be between 1 and total population"))
    }
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

## ---------------------------
## Model Training Functions
## ---------------------------
get_estimated_time <- function(model_type, data_size) {
  base_times <- list(
    "Linear Regression" = 15,
    "Decision Tree" = 45,
    "Random Forest" = 90,
    "Support Vector Regression" = 120,
    "Lasso Regression" = 60,
    "Ridge Regression" = 50
  )
  
  multiplier <- max(1, data_size / 1000)
  estimated_seconds <- base_times[[model_type]] * multiplier
  
  if (estimated_seconds < 60) {
    return(paste(round(estimated_seconds), "seconds"))
  } else {
    return(paste(round(estimated_seconds / 60, 1), "minutes"))
  }
}

create_dummy_model <- function(method = "lm") {
  if (method == "lm") {
    dummy <- list(coefficients = rep(0, 10), fitted.values = 0)
    class(dummy) <- "lm"
  } else {
    dummy <- list(
      method = method,
      finalModel = list(coefficients = rep(0, 10)),
      pred = function(x) rep(0, nrow(x))
    )
    class(dummy) <- "train"
  }
  return(dummy)
}

safe_model_predict <- function(model, newdata) {
  tryCatch({
    if (inherits(model, "lm")) {
      predict(model, newdata = newdata)
    } else if (inherits(model, "train")) {
      predict(model, newdata = newdata)
    } else {
      0
    }
  }, error = function(e) {
    0
  })
}

# Generic model training function
train_models_generic <- function(data, outcome, model_type, progress_callback = NULL) {
  models <- list()
  total_persons <- 180
  
  for (i in 1:total_persons) {
    person <- paste0("P_", i)
    tryCatch({
      person_data <- data %>% filter(Person == person)
      
      if (nrow(person_data) > 5) {
        formula_str <- paste(outcome, "~ SCREEN1_before + SCREEN1_during + SCREEN1_after + 
                            SCREEN2_before + SCREEN2_during + SCREEN2_after + 
                            DIAG_before + DIAG_during + DIAG_after")
        
        if (model_type == "Linear Regression") {
          models[[person]] <- lm(as.formula(formula_str), data = person_data)
        } else {
          method_map <- list(
            "Decision Tree" = "rpart",
            "Random Forest" = "rf",
            "Support Vector Regression" = "svmRadial",
            "Lasso Regression" = "glmnet",
            "Ridge Regression" = "glmnet"
          )
          
          tune_params <- if (model_type %in% c("Lasso Regression", "Ridge Regression")) {
            alpha_val <- if (model_type == "Lasso Regression") 1 else 0
            expand.grid(alpha = alpha_val, lambda = 10^seq(-3, 0, length = 5))
          } else {
            NULL
          }
          
          models[[person]] <- train(
            as.formula(formula_str),
            data = person_data,
            method = method_map[[model_type]],
            tuneGrid = tune_params,
            tuneLength = if (is.null(tune_params)) 2 else NULL,
            trControl = trainControl(method = "cv", number = 3, verboseIter = FALSE)
          )
        }
      } else {
        models[[person]] <- create_dummy_model(ifelse(model_type == "Linear Regression", "lm", "train"))
      }
    }, error = function(e) {
      models[[person]] <- create_dummy_model(ifelse(model_type == "Linear Regression", "lm", "train"))
    })
    
    if (!is.null(progress_callback)) {
      progress_callback(i / total_persons, paste("Training person", i, "of", total_persons))
    }
  }
  return(models)
}

## ---------------------------
## UI Definition
## ---------------------------
ui <- fluidPage(
  titlePanel("Healthcare Screening Intervention Analysis - Portfolio Demo"),
  
  tags$div(
    style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px; padding: 15px; margin-bottom: 20px;",
    tags$strong("Portfolio Demonstration:"),
    tags$p("This application showcases advanced R Shiny development with machine learning integration, 
           complex reactive programming, and dynamic multi-population analysis. The data and parameters 
           have been anonymized for demonstration purposes.", style = "margin-bottom: 0;")
  ),
  
  # Modal for processing notifications
  div(id = "processing-modal", class = "modal", style = "display: none; position: fixed; z-index: 9999; left: 0; top: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.5);",
      div(class = "modal-content", style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); background-color: #007bff; color: white; padding: 40px; border-radius: 15px; text-align: center; box-shadow: 0 8px 32px rgba(0,0,0,0.3);",
          div(style = "font-size: 24px; font-weight: bold; margin-bottom: 20px;",
              span(id = "modal-message", "Processing...")
          ),
          div(style = "font-size: 16px; margin-bottom: 30px;",
              span(id = "modal-detail", "Please wait while we process your request...")
          ),
          div(class = "spinner", style = "border: 4px solid rgba(255,255,255,0.3); border-radius: 50%; border-top: 4px solid white; width: 50px; height: 50px; animation: spin 1s linear infinite; margin: 0 auto;")
      )
  ),
  
  tags$head(
    tags$style(HTML("
      @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
      }
    "))
  ),
  
  tags$script(HTML("
  Shiny.addCustomMessageHandler('showModal', function(data) {
      document.getElementById('modal-message').innerText = data.message;
      document.getElementById('modal-detail').innerText = data.detail;
      document.getElementById('processing-modal').style.display = 'block';
  });
  
  Shiny.addCustomMessageHandler('hideModal', function(data) {
      document.getElementById('processing-modal').style.display = 'none';
  });")),
  
  # Analysis Mode Selection
  fluidRow(
    column(12,
           wellPanel(
             h4("Analysis Mode Selection"),
             radioButtons("analysis_mode", "Choose Analysis Type:",
                          choices = list(
                            "Single Population Analysis" = "single",
                            "Multi-Population Comparative Analysis" = "multi"
                          ),
                          selected = "single",
                          inline = TRUE),
             conditionalPanel(
               condition = "input.analysis_mode == 'multi'",
               p("Multi-population mode allows you to define sub-populations and run separate analyses for each, with comprehensive cross-population comparisons.", 
                 style = "color: #555; font-style: italic;")
             )
           )
    )
  ),
  
  # Single Population Mode
  conditionalPanel(
    condition = "input.analysis_mode == 'single'",
    
    fluidRow(
      column(12,
             wellPanel(
               h4("Global Configuration"),
               fluidRow(
                 column(3,
                        numericInput("global_total_pop", "Total Population",
                                     value = 100000, min = 1000)
                 ),
                 column(3,
                        selectInput("global_model_type", "Model Type:",
                                    choices = c("Linear Regression",
                                                "Decision Tree",
                                                "Random Forest",
                                                "Support Vector Regression",
                                                "Lasso Regression",
                                                "Ridge Regression"),
                                    selected = "Linear Regression")
                 ),
                 column(3,
                        div(style = "margin-top: 25px;",
                            actionButton("use_global_defaults", "Use Default Demographics",
                                         class = "btn-info btn-sm")
                        )
                 ),
                 column(3,
                        div(style = "margin-top: 25px;",
                            actionButton("clear_global", "Clear All Values",
                                         class = "btn-warning btn-sm")
                        )
                 )
               ),
               
               hr(),
               
               h5("Baseline Screening Parameters"),
               fluidRow(
                 column(4,
                        h6("Screening Method 1"),
                        numericInput("global_screen1_before", "Before",
                                     value = 8, min = 0, max = 30)
                 ),
                 column(4,
                        h6("Screening Method 2"),
                        numericInput("global_screen2_before", "Before",
                                     value = 48, min = 30, max = 70)
                 ),
                 column(4,
                        h6("Diagnostic"),
                        numericInput("global_diag_before", "Before",
                                     value = 7, min = 0, max = 90)
                 )
               ),
               
               hr(),
               
               actionButton("toggle_demographics", "Show/Hide Demographics",
                            class = "btn-outline-secondary btn-sm"),
               
               conditionalPanel(
                 condition = "output.show_demographics",
                 hr(),
                 h5("Population Demographics"),
                 fluidRow(
                   column(6,
                          fluidRow(
                            column(6,
                                   h6("Gender Distribution (%)"),
                                   sliderInput("global_male_pct", "Male", min = 0, max = 100, value = 49),
                                   sliderInput("global_female_pct", "Female", min = 0, max = 100, value = 51),
                                   uiOutput("gender_validation")
                            ),
                            column(6,
                                   h6("Category Distribution (%)"),
                                   sliderInput("global_cat_a_pct", "Category A", min = 0, max = 100, value = 60),
                                   sliderInput("global_cat_b_pct", "Category B", min = 0, max = 100, value = 13),
                                   sliderInput("global_cat_c_pct", "Category C", min = 0, max = 100, value = 27),
                                   uiOutput("category_validation")
                            )
                          )
                   ),
                   column(6,
                          h6("Age Group Distribution (%)"),
                          sliderInput("global_age_45_49", "45–49", min = 0, max = 100, value = 17),
                          sliderInput("global_age_50_54", "50–54", min = 0, max = 100, value = 17),
                          sliderInput("global_age_55_59", "55–59", min = 0, max = 100, value = 16),
                          sliderInput("global_age_60_64", "60–64", min = 0, max = 100, value = 16),
                          sliderInput("global_age_65_69", "65–69", min = 0, max = 100, value = 16),
                          sliderInput("global_age_70_74", "70–74", min = 0, max = 100, value = 18),
                          uiOutput("age_validation")
                   )
                 )
               )
             )
      )
    ),
    
    # Scenario Configuration
    fluidRow(
      column(12,
             wellPanel(
               h4("Scenario Configuration"),
               numericInput("num_scenarios", "Number of Scenarios to Compare:",
                            value = 1, min = 1, max = 5, step = 1),
               
               conditionalPanel(
                 condition = "input.num_scenarios >= 1",
                 hr(),
                 h5("Scenario Names"),
                 uiOutput("scenario_names_ui"),
                 actionButton("apply_names", "Apply Scenario Names", class = "btn-info btn-sm")
               )
             )
      )
    ),
    
    # Dynamic Scenario Panels
    uiOutput("scenario_panels"),
    
    # Results
    fluidRow(
      column(12,
             wellPanel(
               h3("Results Comparison"),
               conditionalPanel(
                 condition = "output.baseline_status",
                 div(
                   style = "background-color: #d4edda; border: 2px solid #007bff; border-radius: 5px; padding: 15px; margin: 10px 0; text-align: center;",
                   h5("🔄 Computing baseline predictions...", style = "color: #004085;")
                 )
               ),
               DTOutput("comparison_table"),
               br(),
               downloadButton("download_comparison", "Download Analysis", class = "btn-primary")
             )
      )
    )
  ),
  
  # Note about data requirements
  fluidRow(
    column(12,
           div(
             style = "background-color: #e7f3ff; border-left: 4px solid #2196F3; padding: 15px; margin-top: 20px;",
             h5(icon("info-circle"), " Technical Implementation Notes:", style = "color: #0066cc;"),
             tags$ul(
               tags$li("Demonstrates complex state management with reactive values"),
               tags$li("Integrates 6 machine learning models (Linear, Tree-based, SVM, Regularization)"),
               tags$li("Implements dynamic UI generation based on user selections"),
               tags$li("Features comprehensive input validation and error handling"),
               tags$li("Supports multi-population comparative analysis (not shown in this simplified view)"),
               tags$li("Full version includes model caching, progress tracking, and advanced visualization")
             ),
             tags$p(tags$strong("Note:"), "This demo requires properly formatted CSV data files. 
                    Contact for full implementation details.", style = "margin-top: 10px; color: #666;")
           )
    )
  )
)

## ---------------------------
## Server Logic
## ---------------------------
server <- function(input, output, session) {
  
  # Reactive values
  demographics_visible <- reactiveVal(TRUE)
  scenario_results <- reactiveVal(list())
  scenario_names <- reactiveVal(c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5"))
  baseline_computing <- reactiveVal(FALSE)
  mapping_df <- create_mapping_df()
  
  output$baseline_status <- reactive({
    baseline_computing()
  })
  outputOptions(output, "baseline_status", suspendWhenHidden = FALSE)
  
  output$show_demographics <- reactive({
    demographics_visible()
  })
  outputOptions(output, "show_demographics", suspendWhenHidden = FALSE)
  
  # Demographics toggle
  observeEvent(input$toggle_demographics, {
    current_state <- demographics_visible()
    demographics_visible(!current_state)
  })
  
  # Validation totals
  gender_total <- reactive({
    input$global_male_pct + input$global_female_pct
  })
  
  category_total <- reactive({
    input$global_cat_a_pct + input$global_cat_b_pct + input$global_cat_c_pct
  })
  
  age_total <- reactive({
    input$global_age_45_49 + input$global_age_50_54 + input$global_age_55_59 +
      input$global_age_60_64 + input$global_age_65_69 + input$global_age_70_74
  })
  
  # Validation outputs
  output$gender_validation <- renderUI({
    total <- gender_total()
    if (!is.null(total) && total != 100) {
      div(
        style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 8px;",
        strong("⚠️ Error: ", style = "color: #721c24;"),
        span(paste("Total =", total, "%. Must equal 100%."), style = "color: #721c24;")
      )
    }
  })
  
  output$category_validation <- renderUI({
    total <- category_total()
    if (!is.null(total) && total != 100) {
      div(
        style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 8px;",
        strong("⚠️ Error: ", style = "color: #721c24;"),
        span(paste("Total =", total, "%. Must equal 100%."), style = "color: #721c24;")
      )
    }
  })
  
  output$age_validation <- renderUI({
    total <- age_total()
    if (!is.null(total) && total != 100) {
      div(
        style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 8px;",
        strong("⚠️ Error: ", style = "color: #721c24;"),
        span(paste("Total =", total, "%. Must equal 100%."), style = "color: #721c24;")
      )
    }
  })
  
  # Default demographics button
  observeEvent(input$use_global_defaults, {
    updateSliderInput(session, "global_male_pct", value = 49)
    updateSliderInput(session, "global_female_pct", value = 51)
    updateSliderInput(session, "global_cat_a_pct", value = 60)
    updateSliderInput(session, "global_cat_b_pct", value = 13)
    updateSliderInput(session, "global_cat_c_pct", value = 27)
    updateSliderInput(session, "global_age_45_49", value = 17)
    updateSliderInput(session, "global_age_50_54", value = 17)
    updateSliderInput(session, "global_age_55_59", value = 16)
    updateSliderInput(session, "global_age_60_64", value = 16)
    updateSliderInput(session, "global_age_65_69", value = 16)
    updateSliderInput(session, "global_age_70_74", value = 18)
    showNotification("Default demographics applied!", type = "message")
  })
  
  # Clear button
  observeEvent(input$clear_global, {
    updateNumericInput(session, "global_total_pop", value = 100000)
    updateNumericInput(session, "global_screen1_before", value = 8)
    updateNumericInput(session, "global_screen2_before", value = 48)
    updateNumericInput(session, "global_diag_before", value = 7)
    scenario_results(list())
    showNotification("All values cleared!", type = "message")
  })
  
  # Scenario names UI
  output$scenario_names_ui <- renderUI({
    num_scenarios <- input$num_scenarios
    if (is.null(num_scenarios) || num_scenarios < 1) return(NULL)
    
    current_names <- scenario_names()
    
    name_inputs <- lapply(1:num_scenarios, function(i) {
      fluidRow(
        column(2,
               div(style = "margin-top: 5px;", strong(paste("Scenario", i, ":")))
        ),
        column(10,
               textInput(paste0("scenario_name_", i), NULL,
                         value = if(i <= length(current_names)) current_names[i] else paste("Scenario", i))
        )
      )
    })
    
    do.call(tagList, name_inputs)
  })
  
  # Apply scenario names
  observeEvent(input$apply_names, {
    num_scenarios <- input$num_scenarios
    if (is.null(num_scenarios)) return()
    
    new_names <- sapply(1:num_scenarios, function(i) {
      name_input <- input[[paste0("scenario_name_", i)]]
      if (is.null(name_input) || name_input == "") {
        paste("Scenario", i)
      } else {
        name_input
      }
    })
    
    all_names <- c(new_names, paste("Scenario", (length(new_names) + 1):5))
    scenario_names(all_names[1:5])
    showNotification("Scenario names updated!", type = "message")
  })
  
  get_scenario_name <- function(scenario_num) {
    current_names <- scenario_names()
    if (scenario_num <= length(current_names)) {
      return(current_names[scenario_num])
    } else {
      return(paste("Scenario", scenario_num))
    }
  }
  
  # Dynamic scenario panels
  output$scenario_panels <- renderUI({
    num_scenarios <- input$num_scenarios
    if (is.null(num_scenarios) || num_scenarios < 1) return(NULL)
    
    scenario_panels <- lapply(1:num_scenarios, function(i) {
      scenario_name <- get_scenario_name(i)
      
      fluidRow(
        column(12,
               wellPanel(
                 h4(scenario_name),
                 fluidRow(
                   column(6,
                          h5("During Parameters"),
                          numericInput(paste0("screen1_during_", i), "Screening Method 1 During",
                                       value = 10, min = 0, max = 30),
                          numericInput(paste0("screen2_during_", i), "Screening Method 2 During",
                                       value = 55, min = 30, max = 70),
                          numericInput(paste0("diag_during_", i), "Diagnostic During",
                                       value = 10, min = 0, max = 90)
                   ),
                   column(6,
                          h5("After Parameters"),
                          numericInput(paste0("screen1_after_", i), "Screening Method 1 After",
                                       value = 9, min = 0, max = 30),
                          numericInput(paste0("screen2_after_", i), "Screening Method 2 After",
                                       value = 52, min = 30, max = 70),
                          numericInput(paste0("diag_after_", i), "Diagnostic After",
                                       value = 8, min = 0, max = 90),
                          br(),
                          actionButton(paste0("run_sim_", i),
                                       paste("Run Analysis for", scenario_name),
                                       class = "btn-success",
                                       style = "width: 100%;"),
                          br(), br(),
                          uiOutput(paste0("validation_messages_", i)),
                          verbatimTextOutput(paste0("status_", i))
                   )
                 )
               )
        )
      )
    })
    
    do.call(tagList, scenario_panels)
  })
  
  # Comparison table
  output$comparison_table <- renderDT({
    results <- scenario_results()
    if (length(results) == 0) {
      return(data.frame(
        Message = "Run at least one scenario to see results",
        Info = "Configure parameters above and click 'Run Analysis'"
      ))
    }
    
    comparison_data <- data.frame(
      Scenario = character(),
      Model_Type = character(),
      Population = numeric(),
      Outcome_1 = numeric(),
      Outcome_2 = numeric(),
      Outcome_3 = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (scenario_name in names(results)) {
      result <- results[[scenario_name]]
      comparison_data <- rbind(comparison_data, data.frame(
        Scenario = result$scenario_name,
        Model_Type = result$model_type,
        Population = result$total_pop,
        Outcome_1 = result$outcome1,
        Outcome_2 = result$outcome2,
        Outcome_3 = result$outcome3,
        stringsAsFactors = FALSE
      ))
    }
    
    datatable(comparison_data,
              options = list(
                dom = 't',
                scrollX = TRUE,
                pageLength = -1
              ),
              rownames = FALSE)
  })
  
  # Note: Full implementation would include actual model training and prediction
  # This demo shows the structure without requiring actual data files
  
  showNotification(
    "Portfolio Demo: This version demonstrates the UI/UX design and structure. 
    Full implementation includes ML model training and predictions.",
    duration = 10,
    type = "message"
  )
}

# Run the application
shinyApp(ui, server)