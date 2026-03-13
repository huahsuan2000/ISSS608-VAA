pacman::p_load(
  shiny, shinyWidgets, tidyverse, readr, rpart,
  caret, visNetwork, partykit
)

customer_data_clean <- readr::read_rds("../data/customer_data_clean.rds")

target_var <- "churn_probability"
#predictor = every variable - target variable
predictor_choices <- setdiff(names(customer_data_clean), target_var)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .control-panel {
        background: #f5f5f5;
        padding: 18px;
        border-radius: 10px;
      }

      .metric-row {
        display: flex;
        gap: 16px;
        flex-wrap: wrap;
        margin-bottom: 20px;
      }
      .metric-box {
        background: #f3d55b;
        padding: 8px 16px;
        border-radius: 10px;
        text-align: center;
        width: 240px;
        min-height: 65px;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }

      .metric-box h3 {
        margin: 0;
        font-size: 20px;
        font-weight: 600;
      }

      .metric-box p {
        margin: 6px 0 0 0;
        font-size: 14px;
      }

      .chart-card {
        background: white;
        padding: 14px;
        border-radius: 10px;
        margin-bottom: 16px;
      }

      .section-title {
        margin-top: 4px;
        margin-bottom: 12px;
        font-weight: 600;
      }

      .path-box {
        background: #fafafa;
        border: 1px solid #e6e6e6;
        border-radius: 8px;
        padding: 12px;
      }
    "))
  ),
  
  titlePanel("Decision Tree Model"),
  #width加總起來為12
  fluidRow(
    column(
      width = 3,   
      div(
        class = "control-panel",
        h4("Model Setup"),
        
        pickerInput(
          inputId = "predictors",
          label = "Variable Selection",
          choices = predictor_choices,
          selected = predictor_choices,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `selected-text-format` = "count > 10",
            `none-selected-text` = "Please select variables"
          )
        ),
        
        sliderInput(
          inputId = "split_ratio",
          label = "Train/Test Split Ratio",
          min = 0.5,
          max = 0.9,
          value = 0.8,
          step = 0.05
        ),
        
        sliderInput(
          inputId = "minsplit",
          label = "Minimum Split",
          min = 5,
          max = 50,
          value = 10,
          step = 1
        ),
        
        sliderInput(
          inputId = "maxdepth",
          label = "Maximum Depth",
          min = 1,
          max = 15,
          value = 10,
          step = 1
        ),
        
        sliderInput(
          inputId = "cp",
          label = "Complexity Parameter (CP)",
          min = 0.0001,
          max = 0.05,
          value = 0.001,
          step = 0.0005
        ),
        
        actionButton("build_model", "Build Model"),
        
        br(), br(),
        h4("Parameter Explanation"),
        div(
          class = "path-box",
          tags$p(
            tags$b("Train/Test Split Ratio: "),
            "Controls how the dataset is divided into training and testing sets. For example, 0.8 means 80% of the data is used for training and 20% for testing."
          ),
          tags$p(
            tags$b("Minimum Split: "),
            "Defines the minimum number of observations required in a node before the tree is allowed to split that node."
          ),
          tags$p(
            tags$b("Maximum Depth: "),
            "Sets the maximum number of levels the tree can grow. A deeper tree is more flexible but may also become more complex."
          ),
          tags$p(
            tags$b("Complexity Parameter (CP): "),
            "Controls whether a split is worthwhile. A higher CP makes the tree simpler, while a lower CP allows more splits."
          )
        )
      )
    ),
    column(
      width = 9,
      
      h3(class = "section-title", "Model Performance"),
      div(
        class = "metric-row",
        uiOutput("rmse_box"),
        uiOutput("mae_box"),
        uiOutput("r2_box")
      ),
      
      h3(class = "section-title", "Decision Tree Dashboard"),
      
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          "Model Visualization",
          br(),
          fluidRow(
            column(
              width = 7,
              div(
                class = "chart-card",
                h4("Decision Tree"),
                visNetworkOutput("tree_plot", height = "450px")
              )
            ),
            
            column(
              width = 5,
              div(
                class = "chart-card",
                h4("Predicted vs Actual on Test Data"),
                plotOutput("pred_actual_plot", height = "200px")
              ),
              
              div(
                class = "chart-card",
                h4("Feature Importance of Decision Tree"),
                plotOutput("importance_plot", height = "250px")
              )
            )
          )
        ),
        
        tabPanel(
          "Scenario Prediction",
          br(),
          fluidRow(
            column(
              width = 4,
              div(
                class = "chart-card",
                h4("Input Scenario"),
                p("Enter values for the variables used by the current tree model."),
                uiOutput("scenario_inputs"),
                br(),
                actionButton("predict_case", "Predict Scenario")
              )
            ),
            
            column(
              width = 8,
              div(
                class = "chart-card",
                h4("Prediction Result"),
                uiOutput("scenario_pred_box")
              ),
              
              div(
                class = "chart-card",
                h4("Decision Path"),
                uiOutput("decision_path")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  tree_model <- function(data, predictors, min_split, complexity_parameter, max_depth) {
    model_formula <- as.formula(
      paste(target_var, "~", paste(predictors, collapse = " + "))
    )
    
    rpart(
      formula = model_formula,
      data = data,
      method = "anova",
      control = rpart.control(
        minsplit = min_split,
        cp = complexity_parameter,
        maxdepth = max_depth
      )
    )
  }
  
  # most frequent level for factor(default for scenario input)
  get_factor_default <- function(x) {
    x_non_na <- x[!is.na(x)]
    if (length(x_non_na) == 0) {
      return(levels(x)[1])
    }
    names(sort(table(x_non_na), decreasing = TRUE))[1]
  }
  
  # most frequent value for logical(default for scenario input)
  get_logical_default <- function(x) {
    x_non_na <- x[!is.na(x)]
    if (length(x_non_na) == 0) {
      return(FALSE)
    }
    as.logical(names(sort(table(x_non_na), decreasing = TRUE))[1])
  }
  
  # median for numeric
  get_numeric_default <- function(x) {
    val <- median(x, na.rm = TRUE)
    if (!is.finite(val)) val <- 0
    as.numeric(val)
  }
  
  make_input_ui <- function(var, data) {
    x <- data[[var]]
    input_id <- paste0("case_", var)
    
    if (is.factor(x)) {
      default_val <- get_factor_default(x)
      
      selectInput(
        inputId = input_id,
        label = var,
        choices = levels(x),
        selected = default_val
      )
      
    } else if (is.logical(x)) {
      default_val <- get_logical_default(x)
      
      selectInput(
        inputId = input_id,
        label = var,
        choices = c(TRUE, FALSE),
        selected = default_val
      )
      
    } else {
      default_val <- get_numeric_default(x)
      
      numericInput(
        inputId = input_id,
        label = var,
        value = round(default_val, 2)
      )
    }
  }
  
  build_new_case <- function(all_vars, input_vars, data, input) {
    out <- vector("list", length(all_vars))
    names(out) <- all_vars
    
    # first fill all selected predictors with defaults
    for (var in all_vars) {
      template <- data[[var]]
      
      if (is.factor(template)) {
        default_val <- get_factor_default(template)
        out[[var]] <- factor(default_val, levels = levels(template))
        
      } else if (is.logical(template)) {
        default_val <- get_logical_default(template)
        out[[var]] <- as.logical(default_val)
        
      } else {
        default_val <- get_numeric_default(template)
        out[[var]] <- as.numeric(default_val)
      }
    }
    
    # then overwrite only tree-used variables with user inputs
    for (var in input_vars) {
      val <- input[[paste0("case_", var)]]
      template <- data[[var]]
      
      if (is.factor(template)) {
        out[[var]] <- factor(as.character(val), levels = levels(template))
        
      } else if (is.logical(template)) {
        out[[var]] <- as.logical(val)
        
      } else {
        out[[var]] <- as.numeric(val)
      }
    }
    
    as.data.frame(out, stringsAsFactors = FALSE)
  }
  
  model_results <- eventReactive(input$build_model, {
    req(input$predictors)
    req(length(input$predictors) > 0)
    
    df_model <- customer_data_clean %>%
      select(all_of(c(target_var, input$predictors)))
    
    set.seed(1234)
    
    train_index <- createDataPartition(
      df_model[[target_var]],
      p = input$split_ratio,
      list = FALSE
    )
    
    df_train <- df_model[train_index, , drop = FALSE]
    df_test  <- df_model[-train_index, , drop = FALSE]
    
    fit_tree <- tree_model(
      data = df_train,
      predictors = input$predictors,
      min_split = input$minsplit,
      complexity_parameter = input$cp,
      max_depth = input$maxdepth
    )
    
    df_test$pred_tree <- predict(fit_tree, newdata = df_test)
    
    if (is.null(fit_tree$variable.importance)) {
      tree_importance <- tibble(
        Variable = character(),
        Importance = numeric()
      )
    } else {
      tree_importance <- tibble(
        Variable = names(fit_tree$variable.importance),
        Importance = as.numeric(fit_tree$variable.importance)
      ) %>%
        arrange(desc(Importance))
    }
    
    rmse_value <- sqrt(mean((df_test[[target_var]] - df_test$pred_tree)^2))
    mae_value  <- mean(abs(df_test[[target_var]] - df_test$pred_tree))
    
    r2_value <- if (sd(df_test[[target_var]]) == 0 || sd(df_test$pred_tree) == 0) {
      NA_real_
    } else {
      cor(df_test[[target_var]], df_test$pred_tree)^2
    }
    
    tree_vars <- unique(fit_tree$frame$var[fit_tree$frame$var != "<leaf>"])
    
    list(
      fit_tree = fit_tree,
      df_model = df_model,
      df_test = df_test,
      tree_importance = tree_importance,
      rmse_value = rmse_value,
      mae_value = mae_value,
      r2_value = r2_value,
      tree_vars = tree_vars,
      selected_predictors = input$predictors
    )
  })
  
  scenario_result <- eventReactive(input$predict_case, {
    req(model_results())
    
    input_vars <- model_results()$tree_vars
    all_vars   <- model_results()$selected_predictors
    
    req(length(input_vars) > 0)
    
    new_case <- build_new_case(
      all_vars = all_vars,
      input_vars = input_vars,
      data = model_results()$df_model,
      input = input
    )
    
    pred_value <- as.numeric(
      predict(model_results()$fit_tree, newdata = new_case)
    )
    
    party_model <- as.party(model_results()$fit_tree)
    leaf_node <- as.integer(
      predict(party_model, newdata = new_case, type = "node")
    )
    
    path_steps <- path.rpart(
      model_results()$fit_tree,
      nodes = leaf_node,
      print.it = FALSE
    )[[1]]
    
    list(
      new_case = new_case,
      pred_value = pred_value,
      leaf_node = leaf_node,
      path_steps = path_steps
    )
  })
  
  output$rmse_box <- renderUI({
    req(model_results())
    div(
      class = "metric-box",
      h3(round(model_results()$rmse_value, 4)),
      p("RMSE")
    )
  })
  
  output$mae_box <- renderUI({
    req(model_results())
    div(
      class = "metric-box",
      h3(round(model_results()$mae_value, 4)),
      p("MAE")
    )
  })
  
  output$r2_box <- renderUI({
    req(model_results())
    div(
      class = "metric-box",
      h3(round(model_results()$r2_value, 4)),
      p("R-squared")
    )
  })
  
  output$tree_plot <- renderVisNetwork({
    req(model_results())
    
    visTree(
      model_results()$fit_tree,
      edgesFontSize = 14,
      nodesFontSize = 16,
      width = "100%",
      height = "650px"
    )
  })
  
  output$pred_actual_plot <- renderPlot({
    req(model_results())
    
    ggplot(model_results()$df_test, aes(x = churn_probability, y = pred_tree)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
      labs(
        x = "Actual Churn Probability",
        y = "Predicted Churn Probability"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$importance_plot <- renderPlot({
    req(model_results())
    
    validate(
      need(
        nrow(model_results()$tree_importance) > 0,
        "No variable importance available for this tree."
      )
    )
    
    model_results()$tree_importance %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Variable",
        y = "Importance"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$scenario_inputs <- renderUI({
    req(model_results())
    
    input_vars <- model_results()$tree_vars
    
    if (length(input_vars) == 0) {
      return(tags$p("This tree has no split variables."))
    }
    
    tagList(
      lapply(input_vars, make_input_ui, data = model_results()$df_model)
    )
  })
  
  output$scenario_pred_box <- renderUI({
    req(scenario_result())
    
    div(
      class = "metric-box",
      style = "width: 220px;",
      h3(sprintf("%.2f%%", scenario_result()$pred_value * 100)),
      p("Predicted Churn Probability")
    )
  })
  
  output$decision_path <- renderUI({
    req(scenario_result())
    
    div(
      class = "path-box",
      tags$p(
        tags$b("Terminal node: "),
        scenario_result()$leaf_node
      ),
      tags$ol(
        lapply(scenario_result()$path_steps, function(step) {
          tags$li(step)
        })
      )
    )
  })
}

shinyApp(ui = ui, server = server)