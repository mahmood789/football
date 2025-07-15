# ==============================================================================
# Football Spectatorship & Cardiovascular Events Meta-Analysis App
#
# Description:
# This app performs a comprehensive meta-analysis on the association between
# watching high-stakes football matches and the incidence of acute 
# cardiovascular events. It uses real data curated from published studies.
# The app includes a full suite of advanced analyses and allows for the
# download of publication-quality plots with enhanced visual styling.
#
# Required packages: shiny, meta, dplyr, readr, bslib
# ==============================================================================

# --------------- 1. LOAD PACKAGES ---------------
library(shiny)
library(meta)
library(dplyr)
library(readr)
library(bslib)


# --------------- 2. USER INTERFACE (UI) ---------------
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "yeti"),
  
  titlePanel(
    windowTitle = "Football & CV Events Meta-Analysis",
    title = div(
      icon("futbol", class = "fa-2x", style="color: #2C3E50; margin-right: 15px;"),
      "Football Spectatorship & Cardiovascular Events Meta-Analysis"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4(icon("sliders-h", class="fa-fw"), "Analysis Control Panel"),
      
      # 1. Select Outcome
      selectInput("outcome_select", 
                  label = "Step 1: Choose an Outcome",
                  choices = c("All Cardiovascular Events" = "all_cv",
                              "Myocardial Infarction" = "mi"),
                  selected = "all_cv"),
      
      # 2. Select Analysis Type
      selectInput("analysis_type",
                  label = "Step 2: Choose Analysis Type",
                  choices = c("Overall Analysis", 
                              "Subgroup Analysis", 
                              "Univariate Meta-Regression",
                              "Sensitivity (Leave-One-Out)",
                              "Cumulative (by Year)"),
                  selected = "Overall Analysis"),
      
      # 3. Conditional Panels for Advanced Options
      conditionalPanel(
        condition = "input.analysis_type == 'Subgroup Analysis'",
        selectInput("subgroup_var", "Select Subgroup Variable:",
                    choices = c("Match Outcome" = "match_outcome",
                                "Gender" = "gender"))
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'Univariate Meta-Regression'",
        selectInput("regression_var", "Select Covariate:",
                    choices = c("Year of Publication" = "year"))
      ),
      
      # 4. Run Button
      actionButton("run_analysis", "Run Analysis", class = "btn-primary btn-lg", icon = icon("cogs"), width = "100%"),
      
      hr(),
      
      h4(icon("info-circle"), "About This Tool"),
      p("This tool uses real data from published studies to analyze the association between football spectatorship and cardiovascular events. It supports a full suite of advanced analyses to explore the evidence base thoroughly.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "results_tabs",
        tabPanel(
          "Analysis Plot",
          icon = icon("chart-bar"),
          h3(textOutput("main_plot_title")),
          plotOutput("main_plot", height = "700px"),
          downloadButton("download_main_plot", "Download Plot", class="btn-success"),
          hr(),
          uiOutput("plot_interpretation")
        ),
        tabPanel(
          "Publication Bias",
          icon = icon("filter"),
          h3("Publication Bias Assessment"),
          plotOutput("funnel_plot", height = "550px"),
          downloadButton("download_funnel_plot", "Download Funnel Plot", class="btn-success"),
          hr(),
          h4("Formal Test for Funnel Plot Asymmetry (Egger's Test)"),
          verbatimTextOutput("bias_test_output")
        ),
        tabPanel(
          "Statistical Summary",
          icon = icon("file-alt"),
          h3(textOutput("summary_title")),
          verbatimTextOutput("summary_text")
        )
      )
    )
  )
)


# --------------- 3. SERVER LOGIC ---------------
server <- function(input, output) {
  
  # --- Data Preparation ---
  all_cv_data_string <- "
    study_author,year,log_irr,se_log_irr,gender,match_outcome
    Wilbert-Lampen,2008,0.978,0.068,Both,Mixed
    Witte,2000,0.412,0.161,Men,Loss
    Kirkup,2003,0.247,0.070,Men,Loss
    Carroll,2002,0.223,0.001,Men,Loss
    Berthier,2003,-0.342,0.146,Men,Win
    Katz,2006,0.489,0.206,Both,Mixed
    Niederseer,2013,-0.089,0.110,Both,Mixed
    Barone-Adesi,2010,0.010,0.017,Both,Mixed
    Mahecic,2022,0.140,0.064,Both,Mixed
    Simon,2020,0.049,0.040,Men,Mixed
    Puche,2022,0.223,0.001,Men,Loss
    "
  
  mi_data_string <- "
    study_author,year,log_irr,se_log_irr,gender,match_outcome
    Wilbert-Lampen,2008,0.959,0.040,Both,Mixed
    Witte,2000,0.412,0.161,Men,Loss
    Kirkup,2003,0.247,0.070,Men,Loss
    Carroll,2002,0.223,0.001,Men,Loss
    Berthier,2003,-0.342,0.163,Men,Win
    Barone-Adesi,2010,0.010,0.017,Both,Mixed
    Simon,2020,0.049,0.040,Men,Mixed
    Puche,2022,0.223,0.001,Men,Loss
    "
  
  # --- Reactive Analysis Engine ---
  analysis_results <- eventReactive(input$run_analysis, {
    
    data_string <- switch(input$outcome_select,
                          "all_cv" = all_cv_data_string,
                          "mi" = mi_data_string)
    
    data <- read_csv(data_string, trim_ws = TRUE, show_col_types = FALSE) %>%
      filter(se_log_irr > 0) 
    
    meta_base <- metagen(
      TE = log_irr,
      seTE = se_log_irr,
      studlab = paste(study_author, year),
      data = data,
      sm = "RR", 
      method.random.ci = "HK" 
    )
    
    analysis_obj <- switch(input$analysis_type,
                           "Overall Analysis" = meta_base,
                           "Subgroup Analysis" = update(meta_base, subgroup = data[[input$subgroup_var]]),
                           "Univariate Meta-Regression" = metareg(meta_base, as.formula(paste("~", input$regression_var))),
                           "Sensitivity (Leave-One-Out)" = metainf(meta_base, pooled="random"),
                           "Cumulative (by Year)" = metacum(meta_base, sortvar = year),
                           meta_base
    )
    
    return(list(main = analysis_obj, base = meta_base))
  })
  
  # --- Render Outputs ---
  output$main_plot_title <- renderText({ paste(input$analysis_type, "for", gsub("_", " ", input$outcome_select)) })
  output$summary_title <- renderText({ paste("Statistical Summary:", input$analysis_type) })
  
  main_plot_reactive <- eventReactive(input$run_analysis, {
    results <- analysis_results()
    req(results)
    
    if (input$analysis_type == "Univariate Meta-Regression") {
      bubble(results$main,
             xlab = paste("Covariate:", gsub("_", " ", input$regression_var)),
             ylab = paste("Log Incidence Rate Ratio"),
             main = paste(input$analysis_type, "of", gsub("_", " ", input$outcome_select)),
             cex = 1.2, pch = 19, col.line = "#B3282D")
    } else if (input$analysis_type %in% c("Sensitivity (Leave-One-Out)", "Cumulative (by Year)")) {
      forest(results$main,
             smlab = "Incidence Rate Ratio",
             col.diamond = "#B3282D",
             col.square = "#005A9C",
             xlab = "Incidence Rate Ratio (IRR)")
    } else if (input$analysis_type == "Subgroup Analysis") {
      # This code block for subgroups remains as it was, since it works correctly.
      forest(results$main,
             sortvar = TE,
             prediction = TRUE,
             print.pred = TRUE,
             smlab = "Incidence Rate Ratio (IRR)",
             leftcols = c("studlab"),
             rightcols = c("effect", "ci"),
             leftlabs = c("Study"),
             rightlabs = c("IRR", "[95% CI]"),
             just.studlab = "left",
             just.rightcols = "right",
             colgap.left = "1.5cm",
             colgap.right = "1.5cm",
             print.tau2 = TRUE,
             print.I2 = TRUE,
             col.square = "#005A9C",
             col.diamond = "#B3282D",
             col.diamond.lines = "#B3282D",
             col.predict = "#28A745",
             col.study = "black",
             lwd = 1.5,
             test.overall.random = TRUE,
             test.subgroup.random = TRUE,
             digits = 2,
             cex = 0.95)
    } else {
      # *** DEFINITIVE FIX FOR OVERALL ANALYSIS PLOT ***
      forest(results$main,
             sortvar = TE,
             # Disable all automatic summary text to prevent overlap
             prediction = FALSE, 
             print.tau2 = FALSE,
             print.I2 = FALSE,
             print.Q = FALSE,
             print.p.overall = FALSE,
             # Define columns manually for perfect spacing
             leftcols = c("studlab"),
             rightcols = c("effect", "ci"),
             leftlabs = c("Study"),
             rightlabs = c("IRR", "[95% CI]"),
             just.studlab = "left",
             just.rightcols = "right",
             colgap.left = "1.5cm",
             colgap.right = "1.5cm",
             # Manually specify the summary labels
             smlab = "Overall (Random Effects)",
             text.random = "Random effects model",
             text.common = "Common effect model",
             # Styling
             col.square = "#005A9C",
             col.diamond = "#B3282D",
             col.diamond.lines = "#B3282D",
             digits = 2,
             cex = 0.95)
    }
  })
  output$main_plot <- renderPlot({ main_plot_reactive() })
  
  funnel_plot_reactive <- eventReactive(input$run_analysis, {
    results <- analysis_results()
    req(results)
    
    if (results$base$k >= 10) {
      funnel(results$base,
             studlab = TRUE, 
             cex.studlab = 0.8,
             pos.studlab = 4,
             contour.levels = c(0.9, 0.95, 0.99),
             col.contour = c("#B6B6B6", "#8A8A8A", "#6B6B6B"),
             lwd = 2, cex = 1.5, pch = 21, bg = "#005A9C", col = "white",
             xlab = "Log Incidence Rate Ratio",
             ylab = "Standard Error")
      title(main = paste("Contour-Enhanced Funnel Plot for", gsub("_", " ", input$outcome_select)))
      legend("topright", c("p < 0.1", "p < 0.05", "p < 0.01"), fill = c("#B6B6B6", "#8A8A8A", "#6B6B6B"), bty = "o", title = "Significance Contours")
    } else {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "Funnel plot requires at least 10 studies to be meaningful.", cex = 1.2, font = 2)
    }
  })
  output$funnel_plot <- renderPlot({ funnel_plot_reactive() })
  
  output$summary_text <- renderPrint({
    results <- analysis_results()
    req(results)
    summary(results$main)
  })
  
  output$bias_test_output <- renderPrint({
    results <- analysis_results()
    req(results)
    if (results$base$k >= 10) {
      metabias(results$base, method.bias = "linreg", plotit = FALSE)
    } else {
      "Egger's test requires at least 10 studies to be performed."
    }
  })
  
  output$plot_interpretation <- renderUI({
    switch(input$analysis_type,
           "Univariate Meta-Regression" = HTML("<p><b>Interpretation:</b> This bubble plot shows the relationship between the selected covariate (e.g., study year) and the event rate ratio. Each circle represents a study, with its size proportional to the study's precision. The red line shows the regression trend.</p>"),
           "Sensitivity (Leave-One-Out)" = HTML("<p><b>Interpretation:</b> This plot shows the result of the meta-analysis if each study (listed on the left) were to be excluded. It tests the stability of the overall result. If the pooled estimates (diamonds) are all similar, the result is robust.</p>"),
           "Cumulative (by Year)" = HTML("<p><b>Interpretation:</b> This plot shows how the pooled evidence has evolved over time. Studies are added one by one, sorted by year, showing the cumulative pooled estimate at each step.</p>"),
           HTML("<p><b>Interpretation:</b> Each blue square represents a study's effect size, with its size indicating the study's weight. The horizontal lines are 95% confidence intervals. The red diamond shows the overall pooled effect.</p>")
    )
  })
  
  output$download_main_plot <- downloadHandler(
    filename = function() { paste0(gsub(" ", "_", input$analysis_type), "_", input$outcome_select, ".png") },
    content = function(file) {
      png(file, width = 1200, height = 1000, res = 120) 
      print(main_plot_reactive())
      dev.off()
    }
  )
  
  output$download_funnel_plot <- downloadHandler(
    filename = function() { paste0("Funnel_Plot_", input$outcome_select, ".png") },
    content = function(file) {
      png(file, width = 1000, height = 800, res = 120)
      print(funnel_plot_reactive())
      dev.off()
    }
  )
}

# --------------- 4. RUN THE APP ---------------
shinyApp(ui = ui, server = server)
