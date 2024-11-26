ui <- fluidPage(
  navbarPage(h5("Assignment 2 - Uditha Mayadunna"),
        tabPanel(h5("Raw Data"),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Raw Data Summary",mainPanel(verbatimTextOutput("data_summary1"),verbatimTextOutput("data_summary2"))),
                     tabPanel("Raw Data View", DTOutput("originalData1")),
                     tabPanel("Pairs Plot for Raw Data",
                              selectInput("variableSelectRaw", "Choose Variables:",
                                          choices = names(Ass2Data_pairs), 
                                          selected = names(Ass2Data_pairs)[11:13], 
                                          multiple = TRUE),
                              plotOutput(outputId = "Pairs_Raw",height = "600px", width = "1400px"))
                     
                 ))),
        
        
        tabPanel(h5("Missing Value Explorer"),
           mainPanel(
             tabsetPanel(
               tabPanel("Table", DTOutput("table", height = "600px", width = "400px")),
               tabPanel("Missing Percentatge", plotOutput("missing_plot", height = "600px", width = "1500px")),
               tabPanel("Missing Upset Plot", plotOutput("upset_plot", height = "600px", width = "1500px")),
               tabPanel("Vis_miss Plot", plotOutput("vis_miss_plot",height = "600px", width = "1500px")),
               tabPanel("Vis_dat Plot", plotOutput("vis_dat_plot", height = "600px", width = "1500px")),
               tabPanel("Missing Pattern",
                 mainPanel("", plotOutput("missingnessPattern",height = "600px", width = "1400px"))),
               tabPanel("Correlation of Missingness",
                        mainPanel("", plotOutput("missingCorrelation",height = "600px", width = "1400px")))
               
             )
           )
  ),
  #############################################################################################
  tabPanel(h5("EDA for Numeric Data"),
           mainPanel(
             tabsetPanel(
               tabPanel("Summary",mainPanel(verbatimTextOutput("numeric_data_summary1"),verbatimTextOutput("numeric_data_summary2"))),
                                            
               tabPanel("Raw Data",
                        DT::dataTableOutput(outputId = "numeric_data")),
               tabPanel("Histogram",plotOutput(outputId = "Histogram",height = "600px", width = "1400px")),
               tabPanel("Box Plot", plotOutput(outputId = "Boxplot",height = "600px", width = "1400px"),
                        checkboxInput(inputId = "standardise", label = "Show standardized", value = TRUE),
                        checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                        sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)),
               tabPanel("Corrgram", plotOutput(outputId = "Corrgram",height = "600px", width = "1400px"),
                        checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                        selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                        selectInput(inputId = "Group", label = "Grouping method", choices = list("none" = FALSE, "OLO" = "OLO","GW" = "GW","HC" = "HC"), selected = "OLO")),
               tabPanel("Pairs Plot",plotOutput(outputId = "Pairs",height = "600px", width = "1400px"))
               
               
               ))
           ),
              
  
  #############################################################################################
  tabPanel(h5("EDA for Catogorical Data"),
           mainPanel(
             tabsetPanel(
               tabPanel("Summary",mainPanel(verbatimTextOutput("catogoric_data_summary1"),verbatimTextOutput("catogoric_data_summary2"))),
               tabPanel("Raw Data",
                        DT::dataTableOutput(outputId = "catogoric_data")
               ),
               tabPanel("Visualisation",
                        selectizeInput(inputId = "VariablesA", label = "Show variables:", choices = choicesA, multiple = TRUE, selected = choicesA),
                        
                          plotOutput(outputId = "Mosaic",height = "600px", width = "1400px")
                        
               )
  
  
             ))
  ),
  
  ##############################################################################################
  tabPanel(h5("Dynamic Data Filtering"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("varThreshold", "Threshold for Missing Values in Variables (%)", 
                   value = 50, min = 0, max = 100, step = 5),
      sliderInput("obsThreshold", "Threshold for Missing Values in Observations (%)", 
                   value = 50, min = 0, max = 100, step = 5),
      actionButton("applyThreshold", "Apply Thresholds")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Original Data View", DTOutput("originalData")),
        tabPanel("Filtered Data Summary",mainPanel(verbatimTextOutput("Filtered_data_summary1"),verbatimTextOutput("Filtered_data_summary2"))),
        tabPanel("Filtered Data View", DTOutput("filteredData")),
        tabPanel("Missing Percentatge", plotOutput("missing_plot2", height = "600px", width = "1000px")),
        tabPanel("Missing Upset Plot", plotOutput("upset_plot2", height = "600px", width = "1000px")),
        tabPanel("Vis_miss Plot", plotOutput("vis_miss_plot2",height = "600px", width = "1000px")),
        tabPanel("Vis_dat Plot", plotOutput("vis_dat_plot2", height = "600px", width = "1000px"))
      )
    )
    )),
  
  ##########################################################################################################
  
  

  tabPanel(h5("glmnet Model- Imputation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("varThreshold2", "Variable Threshold (%)", min = 0, max = 100, value = 50),
      sliderInput("obsThreshold2", "Observation Threshold (%)", min = 0, max = 100, value = 50),
      actionButton("applyThreshold2", "Apply Thresholds"),
      selectInput("imputeMethod", "Select Imputation Method", 
                  choices = c( "KNN Imputation", "Median Imputation", "Mean Imputation")),
      numericInput("neighbors", "Number of Neighbors for KNN Imputation", value = 5, min = 1, max = 10),
      checkboxInput("scale", "Apply Scaling to Numeric Predictors", TRUE),
      checkboxInput("center", "Center Numeric Predictors", TRUE),
      sliderInput("iqrMultiplier", "IQR Multiplier for Outlier Detection", min = 0.5, max = 3, value = 1.5, step = 0.1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Predictions Table", tableOutput("predictions")),
        tabPanel("Prediction Plot", plotOutput("predictionPlot",height = "600px", width = "600px")),
        tabPanel("Performance Metrics", verbatimTextOutput("rmseOutput")),
        tabPanel("Residual Plot", plotOutput("residualPlotScatter",height = "600px", width = "1000px")),
        tabPanel("Residual Box-Plot", plotOutput("residualPlot",height = "600px", width = "1000px"))
      )
    )
  )
),

######################################################################################################################
tabPanel(h5("glmnet Model-Partial Deletion"),
         sidebarLayout(
           sidebarPanel(
             sliderInput("varThreshold3", "Variable Threshold (%)", min = 0, max = 100, value = 50),
             sliderInput("obsThreshold3", "Observation Threshold (%)", min = 0, max = 100, value = 50),
             actionButton("applyThreshold3", "Apply Thresholds"),
             checkboxInput("scale3", "Apply Scaling to Numeric Predictors", TRUE),
             checkboxInput("center3", "Center Numeric Predictors", TRUE),
             sliderInput("iqrMultiplier3", "IQR Multiplier for Outlier Detection", min = 0.5, max = 3, value = 1.5, step = 0.1)
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel("Partially Deleted Data Summary",mainPanel(verbatimTextOutput("Partially_summary1"),verbatimTextOutput("Partially_summary2"))),
               tabPanel("Data Table-Partially Deleted", DTOutput("partial")),
               tabPanel("Predictions Table", tableOutput("predictions2")),
               tabPanel("Prediction Plot", plotOutput("predictionPlot2",height = "600px", width = "600px")),
               tabPanel("Performance Metrics", verbatimTextOutput("rmseOutput2")),
               tabPanel("Residual Plot", plotOutput("residualPlotScatter2",height = "600px", width = "1000px")),
               tabPanel("Residual Box-Plot", plotOutput("residualPlot2",height = "600px", width = "1000px"))
             )
           )
         )
)


#################################################################################################
  ))




