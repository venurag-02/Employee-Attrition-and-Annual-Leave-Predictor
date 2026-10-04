#===============================================================================
#         R SHINY DASHBOARD: HR ATTRITION & ANNUAL LEAVE PREDICTOR 2026
#===============================================================================

# Core Shiny & UI Packages
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(bsicons)

# Data Manipulation & I/O
library(tidyverse)
library(readr)
library(dplyr)

# Modeling & Tidymodels Framework
library(tidymodels)
library(parsnip)
library(workflows)

# Underlying Model Engines (Required for predict.model_fit execution)
library(glmnet)      # Engine for regularized logistic regression
library(poissonreg)  # Engine for Poisson count models
library(stats)       # Base statistical functions

#Helper function to generate pie/donut chart ggplot objects

render_pie_chart <- function(pct, title, fill_color = "#2C3E50") {
  pct <- max(0, min(1, pct, na.rm = TRUE)) #Ensures that the percentages are between 0-100.
  df <- data.frame(category = c("Target", "Remaining"), #Synthesizes a two-row dataset where one is the target value and the other is the remaining after taking away the target from the total.
                   count = c(pct, 1 - pct))
  
  
  #Building the donut charts, first bar charts are made and converted to donuts.
  
  ggplot(df, aes(x = 2, y = count, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +     #Wrapping the y-axis to a circle. 
    xlim(0.5, 2.5) +               #Hollowing out the center.
    scale_fill_manual(values = c("Target" = fill_color, "Remaining" = "#ECF0F1")) +
    annotate(
      "text", x = 0.5, y = 0,
      label = paste0(round(pct * 100, 1), "%"),
      size = 5, fontface = "bold"
    ) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(
        hjust = 0.5, size = 11, face = "bold", margin = margin(b = 5)
      )
    ) +
    labs(title = title)
}

# ------------------------------------------------------------------------------
# 1. USER INTERFACE (UI)
# ------------------------------------------------------------------------------


#Building the navigation bar and the rest of the ui.

ui <- page_navbar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50"
  ),
  title = "ATTRITION & ANNUAL LEAVE PREDICTOR",
  
  
  #Using custom CSS styles and hidden DOM elements to the page header.
  
  header = tags$head(
    useShinyjs(), # Initialize shinyjs JavaScript functions across the webap.
    
    # ---------------------------------------------------------------------------
    # ADD CLIENT-SIDE IDLE TIMEOUT SCRIPT HERE
    # ---------------------------------------------------------------------------
    tags$script(HTML("
      var idleTime = 0;
      $(document).ready(function () {
          var idleInterval = setInterval(timerIncrement, 60000); // Check every 1 minute
          $(this).mousemove(function (e) { idleTime = 0; });
          $(this).keypress(function (e) { idleTime = 0; });
      });

      function timerIncrement() {
          idleTime = idleTime + 1;
          if (idleTime >= 10) { // 10 minutes of inactivity
              Shiny.unbindAll();
              alert('Session disconnected due to inactivity to preserve server hours.');
              window.location.reload();
          }
      }
    ")),
    # ---------------------------------------------------------------------------
    
    tags$style(HTML("
      /* Hide print container on screen */
      #print_report_container {
        display: none;
      }
      
      /* Show ONLY print container during print */
      @media print {
        body > *:not(#print_report_container) {
          display: none !important;
        }
        #print_report_container {
          display: block !important;
          position: absolute;
          left: 0;
          top: 0;
          width: 100%;
        }
      }
    ")),
    
    
    #Hidden place holder for the save/print pdf option where a custom layout is used instead of taking a screenshot.
    
    div(id = "print_report_container", uiOutput("printable_report"))
  ),
  
  
  #Navigation bar action button placement and triggers.
  
  nav_spacer(),
  nav_item(
    div(
      class = "d-flex gap-2 align-items-center me-3",
      actionButton("save_btn", "Save", class = "btn-success btn-sm"),
      actionButton(
        "pdf_btn",
        "Print/Save PDF",
        class = "btn-outline-secondary btn-sm"
      )
    )
  ),
  
  #Tab 1: Analytics dashboard ui designing.
  nav_panel(
    title = "Analytics Dashboard",
    
    
    #Sidebar for the "Factors Panel".
    
    layout_sidebar(
      fillable = FALSE,
      sidebar_position = "right",
      
      sidebar = sidebar(
        width = 380,
        card(
          style = "height: 100%; display: flex; flex-direction: column;",
          card_header("Factors"),
          card_body(
            style = "flex: 1; overflow-y: auto;",
            
            
            #Action button to stop the dashboard from updating at every value when the sliders are being adjusted.
            
            actionButton(
              "apply_factors",
              "Apply Changes",
              class = "btn-primary w-100 mt-3",
              icon = icon("calculator")
            ),
            
            hr(style = "margin-top: 0; margin-bottom: 15px;"),
            
            
            #All the factors that could affect the attrition rate and the number of annual leaves taken. Certain factors such as field of education, job role, etc. are removed since it could differ at each organisation.
            
            selectInput("factor_overtime", "Overtime Status:", choices = c("No", "Yes"), selected = "No"),
            selectInput("factor_marital_status", "Marital Status:", choices = c("Married", "Single", "Divorced"), selected = "Married"),
            sliderInput("factor_age", "Employee Age:", min = 18, max = 65, value = 35),
            selectInput("factor_gender", "Gender:", choices = c("Male", "Female"), selected = "Male"),
            selectInput("factor_business_travel", "Travel Status:", 
                        choices = c(
                          "Travel Rarely" = "Travel_Rarely", 
                          "Travel Frequently" = "Travel_Frequently", 
                          "Non Travel" = "Non-Travel"
                        ), 
                        selected = "Travel_Rarely"),
            selectInput("factor_job_satisfaction", "Job Satisfaction (1-4):", choices = 1:4, selected = 3),
            selectInput("factor_wlb", "Work Life Balance (1-4):", choices = 1:4, selected = 3),
            selectInput("factor_relationship_satisfaction", "Relationship Satisfaction (1-4):", choices = 1:4, selected = 3),
            selectInput("factor_env_satisfaction", "Environment Satisfaction (1-4):", choices = 1:4, selected = 3),
            sliderInput("factor_distance", "Distance From Home (km):", min = 1, max = 50, value = 10),
            selectInput("factor_education", "Education Level (1-5):", choices = 1:5, selected = 3),
            selectInput("factor_job_involvement", "Job Involvement (1-4):", choices = 1:4, selected = 3),
            selectInput("factor_job_level", "Job Level (1-5):", choices = 1:5, selected = 1),
            sliderInput("factor_monthly_income", "Monthly Income:", min = 999, max = 20000, value = 2000),
            sliderInput("factor_num_companies_worked", "Number of Companies Worked:", min = 0, max = 9, value = 1),
            sliderInput("factor_percent_salary_hike", "Salary Hike Percentage:", min = 11, max = 25, value = 11),
            sliderInput("factor_stock_option_level", "Stock Option:", min = 0, max = 3, value = 0),
            sliderInput("factor_total_working_years", "Total Working Years:", min = 0, max = 40, value = 5),
            sliderInput("factor_years_at_company", "Years Working at Company:", min = 0, max = 40, value = 0),
            sliderInput("factor_years_in_current_role", "Years Working at Current Role:", min = 0, max = 18, value = 0),
            sliderInput("factor_years_since_last_promotion", "Years Since Last Promotion:", min = 0, max = 15, value = 0),
            sliderInput("factor_years_with_curr_manager", "Years With Current Manager:", min = 0, max = 17, value = 0)
          )
        )
      ),
      
      
      #Three value boxes showing the current attrition, risk level and number of annual leaves this quarter.
      
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title = "Current Calculated Attrition Rate",
          value = textOutput("kpi_attrition_rate"),
          showcase = bsicons::bs_icon("percent"),
          theme = "primary"
        ),
        uiOutput("kpi_risk_box", fill = TRUE),
        value_box(
          title = "Current Calculated Annual Leave (Q1)",
          value = textOutput("kpi_leaves_q1"),
          showcase = bsicons::bs_icon("calendar-event"),
          theme = "info"
        )
      ),
      
      
      #Card created to hold the donut charts which shows the current and predicted value in the next 3 quarters for both attrition and annual leaves.
      
      card(
        card_header("Current Attrition and Predicted Future Attrition"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          plotOutput("pie_att_curr", height = "180px"),
          plotOutput("pie_att_q2", height = "180px"),
          plotOutput("pie_att_q3", height = "180px"),
          plotOutput("pie_att_q4", height = "180px")
        )
      ),
      
      card(
        card_header("Probability of Employee Taking at Least One Leave at Each Quarter"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          plotOutput("pie_leave_curr", height = "180px"),
          plotOutput("pie_leave_q2", height = "180px"),
          plotOutput("pie_leave_q3", height = "180px"),
          plotOutput("pie_leave_q4", height = "180px")
        )
      )
    )
  ),
  
  
  #Tab 2: History tab which shows the "save"/ed scenarios. User doesn't have to worry about changing factors. This also has the option to download the csv for later use.
  
  nav_panel(
    title = "History",
    card(
      card_header("Saved Scenario Runs"),
      DTOutput("history_table"),
      card_footer(
        downloadButton("csv_btn", "Download History CSV", class = "btn-outline-primary btn-sm")
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 2. BACKEND SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  #A. Load models and data from previously run ml and simulations script. 
  models <- reactive({
    req(file.exists("models/log_fit.rds"),
        file.exists("models/poisson_fit.rds"))
    list(
      logistic = readRDS("models/log_fit.rds"),
      poisson  = readRDS("models/poisson_fit.rds"),
      base_df  = readRDS("outputs/final_analytics_dataset.rds")
    )
  })
  
  #B. Single employee synthetic inputs created for custom scenarios.
  synthetic_employee <- eventReactive(
    input$apply_factors, 
    valueExpr = {
      req(models())
      
      base <- models()$base_df
      df <- base |> slice(1)
      
      
      #Factor levels are defined using the original training dataset.
      
      env_levels  <- levels(base$environment_satisfaction)
      job_levels  <- levels(base$job_satisfaction)
      wlb_levels  <- levels(base$work_life_balance)
      ot_levels   <- levels(base$over_time)
      mar_levels  <- levels(base$marital_status)
      rel_levels  <- levels(base$relationship_satisfaction)
      bus_levels  <- levels(base$business_travel)
      edu_levels  <- levels(base$education)
      gen_levels  <- levels(base$gender)
      jobi_levels <- levels(base$job_involvement)
      jobl_levels <- levels(base$job_level)
      
      
      #Ensuring the ui factors align correctly.
      
      df |> mutate(
        
        #Categorical factors.
        
        over_time       = factor(input$factor_overtime, levels = ot_levels),
        marital_status  = factor(input$factor_marital_status, levels = mar_levels),
        business_travel = factor(input$factor_business_travel, levels = bus_levels),
        gender          = factor(input$factor_gender, levels = gen_levels),
        
        #Ordinal factors.
        
        job_satisfaction         = factor(as.character(input$factor_job_satisfaction), levels = job_levels, ordered = is.ordered(base$job_satisfaction)),
        environment_satisfaction = factor(as.character(input$factor_env_satisfaction), levels = env_levels, ordered = is.ordered(base$environment_satisfaction)),
        work_life_balance        = factor(as.character(input$factor_wlb), levels = wlb_levels, ordered = is.ordered(base$work_life_balance)),
        relationship_satisfaction = factor(as.character(input$factor_relationship_satisfaction), levels = rel_levels, ordered = is.ordered(base$relationship_satisfaction)),
        education                = factor(as.character(input$factor_education), levels = edu_levels, ordered = is.ordered(base$education)),
        job_involvement          = factor(as.character(input$factor_job_involvement), levels = jobi_levels, ordered = is.ordered(base$job_involvement)),
        job_level                = factor(as.character(input$factor_job_level), levels = jobl_levels, ordered = is.ordered(base$job_level)),
        
        
        #Numeric factors.
        
        age                        = as.numeric(input$factor_age),
        distance_from_home         = as.numeric(input$factor_distance),
        monthly_income             = as.numeric(input$factor_monthly_income),
        num_companies_worked       = as.numeric(input$factor_num_companies_worked),
        percent_salary_hike        = as.numeric(input$factor_percent_salary_hike),
        stock_option_level         = as.numeric(input$factor_stock_option_level),
        total_working_years        = as.numeric(input$factor_total_working_years),
        years_at_company           = as.numeric(input$factor_years_at_company),
        years_in_current_role      = as.numeric(input$factor_years_in_current_role),
        years_since_last_promotion = as.numeric(input$factor_years_since_last_promotion),
        years_with_curr_manager    = as.numeric(input$factor_years_with_curr_manager)
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )
  
  # C. Multi-quarter projection calculations.
  predictions <- reactive({
    req(synthetic_employee())
    
    emp_data <- synthetic_employee()
    log_mod  <- models()$logistic
    poi_mod  <- models()$poisson
    
    
    #Predicting the current attrition level using pretrained models.
    
    log_pred <- predict(log_mod, new_data = emp_data, type = "prob")
    
    p_attrition_base <- if (".pred_Yes" %in% names(log_pred)) {
      log_pred$.pred_Yes
    } else if (".pred_1" %in% names(log_pred)) {
      log_pred$.pred_1
    } else {
      log_pred[[1]]
    }
    
    
    #Predicting the current number of leaves using pretrained models.
    
    expected_leaves_base <- predict(poi_mod, new_data = emp_data, type = "numeric")$.pred
    
    p_attrition_base     <- as.numeric(p_attrition_base)
    expected_leaves_base <- as.numeric(expected_leaves_base)
    
    
    #Predicting the next 3 quarters.
    
    p_att_q1 <- p_attrition_base
    p_att_q2 <- min(1, p_attrition_base * 1.08)
    p_att_q3 <- min(1, p_attrition_base * 1.15)
    p_att_q4 <- min(1, p_attrition_base * 1.22)
    
    prob_leave_q1 <- 1 - exp(-expected_leaves_base)
    prob_leave_q2 <- 1 - exp(-(expected_leaves_base * 1.05))
    prob_leave_q3 <- 1 - exp(-(expected_leaves_base * 1.10))
    prob_leave_q4 <- 1 - exp(-(expected_leaves_base * 0.95))
    
    
    #Set the ranges for the risk level indicator.
    
    risk <- case_when(
      p_att_q1 > 0.75 ~ "High Risk",
      p_att_q1 > 0.50 ~ "Medium Risk",
      TRUE            ~ "Low Risk"
    )
    
    list(
      attrition      = p_attrition_base,
      att_q1         = p_att_q1,
      att_q2         = p_att_q2,
      att_q3         = p_att_q3,
      att_q4         = p_att_q4,
      leave_q1       = prob_leave_q1,
      leave_q2       = prob_leave_q2,
      leave_q3       = prob_leave_q3,
      leave_q4       = prob_leave_q4,
      leaves_days_q1 = round(expected_leaves_base, 1),
      risk_level     = risk
    )
  })
  
  
  #Outputs for attrition, annual leaves and risk level. Percentage conversions.
  
  output$kpi_attrition_rate <- renderText({
    paste0(round(predictions()$att_q1 * 100, 1), "%")
  })
  
  output$kpi_leaves_q1 <- renderText({
    paste0(predictions()$leaves_days_q1, " Days")
  })
  
  output$kpi_risk_box <- renderUI({
    risk <- predictions()$risk_level
    theme_color <- case_when(
      risk == "High Risk"   ~ "danger",     #colour code for the risk indicator.
      risk == "Medium Risk" ~ "warning",
      TRUE                  ~ "success"
    )
    value_box(
      title = "Risk Level",
      value = risk,
      showcase = bsicons::bs_icon("exclamation-triangle-fill"),
      theme = theme_color
    )
  })
  
  #Donut chart customization.
  
  output$pie_att_curr <- renderPlot({ render_pie_chart(predictions()$att_q1, "Current", "#E74C3C") })
  output$pie_att_q2   <- renderPlot({ render_pie_chart(predictions()$att_q2, "Next Quarter", "#E74C3C") })
  output$pie_att_q3   <- renderPlot({ render_pie_chart(predictions()$att_q3, "After 2 Quarters", "#E74C3C") })
  output$pie_att_q4   <- renderPlot({ render_pie_chart(predictions()$att_q4, "After 3 Quarters", "#E74C3C") })
  
  output$pie_leave_curr <- renderPlot({ render_pie_chart(predictions()$leave_q1, "Current", "#3498DB") })
  output$pie_leave_q2   <- renderPlot({ render_pie_chart(predictions()$leave_q2, "Next Quarter", "#3498DB") })
  output$pie_leave_q3   <- renderPlot({ render_pie_chart(predictions()$leave_q3, "After 2 Quarters", "#3498DB") })
  output$pie_leave_q4   <- renderPlot({ render_pie_chart(predictions()$leave_q4, "After 3 Quarters", "#3498DB") })
  
  
  #Constructs a printable html assessment report with all important info. from the dashboard. Including the quarterly predictions and factors.
  
  output$printable_report <- renderUI({
    preds <- predictions()
    
    tagList(
      div(
        style = "padding: 20px; font-family: Arial, sans-serif;",
        div(
          style = "border-bottom: 2px solid #2C3E50; padding-bottom: 10px; margin-bottom: 20px;",
          h2("Attrition & Annual Leave Assessment Report", style = "color: #2C3E50; margin: 0;"),
          p(style = "color: #7F8C8D; margin-top: 5px;", paste(
            "Generated on:", format(Sys.time(), "%B %d, %Y - %H:%M:%S")
          ))
        ),
        
        h3("Key Risk Summary", style = "color: #2C3E50; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
        div(
          style = "display: flex; justify-content: space-between; margin-bottom: 25px; gap: 15px;",
          div(
            style = "flex: 1; border: 1px solid #bdc3c7; padding: 15px; border-radius: 6px; text-align: center; background-color: #f8f9fa;",
            h5("Current Calculated Attrition", style = "margin: 0; color: #555;"),
            h2(paste0(round(preds$att_q1 * 100, 1), "%"), style = "margin: 10px 0 0 0; color: #E74C3C;")
          ),
          div(
            style = "flex: 1; border: 1px solid #bdc3c7; padding: 15px; border-radius: 6px; text-align: center; background-color: #f8f9fa;",
            h5("Risk Level", style = "margin: 0; color: #555;"),
            h2(
              preds$risk_level,
              style = paste0(
                "margin: 10px 0 0 0; color: ",
                if (preds$risk_level == "High Risk")
                  "#c0392b"
                else if (preds$risk_level == "Medium Risk")
                  "#d35400"
                else
                  "#27ae60",
                ";"
              )
            )
          ),
          div(
            style = "flex: 1; border: 1px solid #bdc3c7; padding: 15px; border-radius: 6px; text-align: center; background-color: #f8f9fa;",
            h5("Annual Leave (Q1)", style = "margin: 0; color: #555;"),
            h2(paste0(preds$leaves_days_q1, " Days"), style = "margin: 10px 0 0 0; color: #2980b9;")
          )
        ),
        
        h3("4-Quarter Predictions Breakdown", style = "color: #2C3E50; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
        tags$table(
          style = "width: 100%; border-collapse: collapse; margin-bottom: 25px;",
          tags$thead(
            tags$tr(
              style = "background-color: #2C3E50; color: white;",
              tags$th(style = "padding: 10px; text-align: left; border: 1px solid #ddd;", "Metric"),
              tags$th(style = "padding: 10px; text-align: center; border: 1px solid #ddd;", "Current / Q1"),
              tags$th(style = "padding: 10px; text-align: center; border: 1px solid #ddd;", "Q2 Projection"),
              tags$th(style = "padding: 10px; text-align: center; border: 1px solid #ddd;", "Q3 Projection"),
              tags$th(style = "padding: 10px; text-align: center; border: 1px solid #ddd;", "Q4 Projection")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(style = "padding: 8px; border: 1px solid #ddd; font-weight: bold;", "Attrition Probability"),
              tags$td(style = "padding: 8px; border: 1px solid #ddd; text-align: center;", paste0(round(preds$att_q1 * 100, 1), "%")),
              tags$td(style = "padding: 8px; border: 1px solid #ddd; text-align: center;", paste0(round(preds$att_q2 * 100, 1), "%")),
              tags$td(style = "padding: 8px; border: 1px solid #ddd; text-align: center;", paste0(round(preds$att_q3 * 100, 1), "%")),
              tags$td(style = "padding: 8px; border: 1px solid #ddd; text-align: center;", paste0(round(preds$att_q4 * 100, 1), "%"))
            ),
            tags$tr(
              style = "background-color: #f9f9f9;",
              tags$td(style = "padding: 8px; border: 1px solid #ddd; font-weight: bold;", "Leave Taking Probability (>= 1 Day)"),
              tags$td(style = "padding: 8px; border: 1px solid #ddd; text-align: center;", paste0(round(preds$leave_q1 * 100, 1), "%")),
              tags$td(style = "padding: 8px; border: 1px solid #ddd; text-align: center;", paste0(round(preds$leave_q2 * 100, 1), "%")),
              tags$td(style = "padding: 8px; border: 1px solid #ddd; text-align: center;", paste0(round(preds$leave_q3 * 100, 1), "%")),
              tags$td(style = "padding: 8px; border: 1px solid #ddd; text-align: center;", paste0(round(preds$leave_q4 * 100, 1), "%"))
            )
          )
        ),
        
        h3("Employee Input Factors", style = "color: #2C3E50; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
        tags$table(
          style = "width: 100%; border-collapse: collapse;",
          tags$tbody(
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Overtime Status:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", input$factor_overtime),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Marital Status:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", input$factor_marital_status)
            ),
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Age:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", input$factor_age),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Gender:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", input$factor_gender)
            ),
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Travel Status:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", input$factor_business_travel),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Job Satisfaction:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste(input$factor_job_satisfaction, "/ 4"))
            ),
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Work Life Balance:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste(input$factor_wlb, "/ 4")),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Environment Satisfaction:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste(input$factor_env_satisfaction, "/ 4"))
            ),
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Monthly Income:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste0("$", input$factor_monthly_income)),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Total Working Years:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", input$factor_total_working_years)
            ),
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Relationship Satisfaction:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste(input$factor_relationship_satisfaction, "/ 4")),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Distance From Home:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste0(input$factor_distance, "km"))
            ), 
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Education Level:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste(input$factor_education, "/ 5")),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Job Involvement:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste(input$factor_job_involvement, "/ 4"))
            ), 
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Job Level:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste(input$factor_job_level, "/ 5")),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "No. of Companies Worked:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste0(input$factor_num_companies_worked))
            ),
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Salary Hike Percentage:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste0(input$factor_percent_salary_hike, "%")),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Stock Option Level:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste0(input$factor_stock_option_level))
            ), 
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Years at Company:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste0(input$factor_years_at_company)),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Years at Current Role:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste0(input$factor_years_in_current_role))
            ),
            tags$tr(
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Years Since Last Promotion:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste0(input$factor_years_since_last_promotion)),
              tags$td(style = "padding: 6px; border: 1px solid #ddd; font-weight: bold;", "Years with Current Manager:"),
              tags$td(style = "padding: 6px; border: 1px solid #ddd;", paste0(input$factor_years_with_curr_manager))
            )
          )
        )
      )
    )
  })
  
  #Ensures the printable format keeps working in the background while the user is customising to give instant preview.
  
  outputOptions(output, "printable_report", suspendWhenHidden = FALSE)
  
  #Action button trigger.
  
  observeEvent(input$pdf_btn, {
    shinyjs::runjs("window.print();")
  })
  
  
  #Scenario saving option where all the info. in the dashboard is saved in the history tab and is instantly ready for a csv download.
  
  history_store <- reactiveVal(
    tibble(
      Timestamp = character(),
      Risk_Level = character(),
      
      Attrition_Rate_Q1           = character(),
      Attrition_Rate_Q2           = character(),
      Attrition_Rate_Q3           = character(),
      Attrition_Rate_Q4           = character(),
      
      Expected_Leaves_Q1_Days     = numeric(),
      Leave_Prob_Q1               = character(),
      Leave_Prob_Q2               = character(),
      Leave_Prob_Q3               = character(),
      Leave_Prob_Q4               = character(),
      
      Overtime                    = character(),
      Marital_Status              = character(),
      Age                         = numeric(),
      Gender                      = character(),
      Business_Travel             = character(),
      Job_Satisfaction            = character(),
      Work_Life_Balance           = character(),
      Relationship_Satisfaction   = character(),
      Environment_Satisfaction    = character(),
      Distance_From_Home_KM       = numeric(),
      Education_Level             = character(),
      Job_Involvement             = character(),
      Job_Level                   = character(),
      Monthly_Income              = numeric(),
      Num_Companies_Worked        = numeric(),
      Percent_Salary_Hike         = numeric(),
      Stock_Option_Level          = numeric(),
      Total_Working_Years         = numeric(),
      Years_At_Company            = numeric(),
      Years_In_Current_Role       = numeric(),
      Years_Since_Last_Promotion  = numeric(),
      Years_With_Curr_Manager     = numeric()
    )
  )
  
  observeEvent(input$save_btn, {
    preds <- predictions()
    
    new_entry <- tibble(
      Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      Risk_Level = preds$risk_level,
      
      Attrition_Rate_Q1           = paste0(round(preds$att_q1 * 100, 1), "%"),
      Attrition_Rate_Q2           = paste0(round(preds$att_q2 * 100, 1), "%"),
      Attrition_Rate_Q3           = paste0(round(preds$att_q3 * 100, 1), "%"),
      Attrition_Rate_Q4           = paste0(round(preds$att_q4 * 100, 1), "%"),
      
      Expected_Leaves_Q1_Days     = preds$leaves_days_q1,
      Leave_Prob_Q1               = paste0(round(preds$leave_q1 * 100, 1), "%"),
      Leave_Prob_Q2               = paste0(round(preds$leave_q2 * 100, 1), "%"),
      Leave_Prob_Q3               = paste0(round(preds$leave_q3 * 100, 1), "%"),
      Leave_Prob_Q4               = paste0(round(preds$leave_q4 * 100, 1), "%"),
      
      Overtime                    = input$factor_overtime,
      Marital_Status              = input$factor_marital_status,
      Age                         = as.numeric(input$factor_age),
      Gender                      = input$factor_gender,
      Business_Travel             = input$factor_business_travel,
      Job_Satisfaction            = as.character(input$factor_job_satisfaction),
      Work_Life_Balance           = as.character(input$factor_wlb),
      Relationship_Satisfaction   = as.character(input$factor_relationship_satisfaction),
      Environment_Satisfaction    = as.character(input$factor_env_satisfaction),
      Distance_From_Home_KM       = as.numeric(input$factor_distance),
      Education_Level             = as.character(input$factor_education),
      Job_Involvement             = as.character(input$factor_job_involvement),
      Job_Level                   = as.character(input$factor_job_level),
      Monthly_Income              = as.numeric(input$factor_monthly_income),
      Num_Companies_Worked        = as.numeric(input$factor_num_companies_worked),
      Percent_Salary_Hike         = as.numeric(input$factor_percent_salary_hike),
      Stock_Option_Level          = as.numeric(input$factor_stock_option_level),
      Total_Working_Years         = as.numeric(input$factor_total_working_years),
      Years_At_Company            = as.numeric(input$factor_years_at_company),
      Years_In_Current_Role       = as.numeric(input$factor_years_in_current_role),
      Years_Since_Last_Promotion  = as.numeric(input$factor_years_since_last_promotion),
      Years_With_Curr_Manager     = as.numeric(input$factor_years_with_curr_manager)
    )
    
    history_store(bind_rows(history_store(), new_entry))
  })
  
  #Layout for the history tab.
  
  output$history_table <- renderDT({
    datatable(history_store(), options = list(pageLength = 5, dom = 'tp', scrollX = TRUE))
  })
  
  #Download handler for exporting the data as csv.
  
  output$csv_btn <- downloadHandler(
    filename = function() {
      paste0("hr_risk_scenario_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      readr::write_csv(history_store(), file)
    }
  )
}

# ------------------------------------------------------------------------------
# 3. LAUNCH APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
