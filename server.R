server <- function(input, output,session) {
  
  Without_Process_Data <- reactive({
    data <- Ass2Data
    
    return(data)
  })
  
  processedData <- reactive({
    data <- data
    
    return(data)
  })
  
  ########################################################################################
  output$data_summary1 <- renderPrint({
    str(Without_Process_Data())
  })
  
  
  output$data_summary2 <- renderPrint({
    summary(Without_Process_Data())
  })
  output$originalData1 <- renderDT({
    datatable(Without_Process_Data())
  })
  
  observe({
    
    vars <- input$variableSelectRaw
    
    
    if(length(vars) < 2) {
      
      return()
    }
    
    
    output$Pairs_Raw <- renderPlot({
      
      valid_columns <- vars %in% names(Ass2Data_pairs)
      if(all(valid_columns)) {
        ggpairs(data = Ass2Data_pairs[, vars, drop = FALSE], 
                title = "Pairs plot of Selected Data")+
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        
      }
    })
  })
  
  
  
  ############################################################################################
  output$table <- renderDT({
    datatable(Without_Process_Data())
  })
  
  output$missing_plot <- renderPlot({
    missing_percentages <- colSums(is.na(Without_Process_Data())) / nrow(Without_Process_Data()) * 100
    missing_data_df <- data.frame(Variable = names(missing_percentages), MissingPercentage = missing_percentages)
    
    ggplot(missing_data_df, aes(x = reorder(Variable, MissingPercentage), y = MissingPercentage, fill = MissingPercentage)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = "green", high = "red") + 
      labs(x = "Variable", y = "Percentage of Missing Data", title = "Missing Data by Variable")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$upset_plot <- renderPlot({
    gg_miss_upset(Without_Process_Data(),nsets=7)
  })
  
  output$vis_miss_plot <- renderPlot({
    vis_miss(Without_Process_Data())
  })
  
  output$vis_dat_plot <- renderPlot({
    vis_dat(Without_Process_Data())
  }, res = 100)
  
  output$missingnessPattern <- renderPlot({
    
    data_missing <-Without_Process_Data()
    data_missing$missingness <- apply(X = is.na(data_missing), MARGIN = 1, FUN = sum)
    tree <- caret::train(missingness ~ .-CODE-OBS_TYPE, 
                         data = data_missing, 
                         method = "rpart", 
                         na.action = na.rpart)
    rpart.plot(tree$finalModel, 
               main = "Predicting the number of missing variables in an observation",
               sub = "Check whether the outcome variable is an important variable",
               roundint = TRUE, 
               clip.facs = TRUE)
    
  })
  
  output$missingCorrelation <- renderPlot({
    corrgram::corrgram(cor(corr_data), order = "OLO", abs = TRUE)
    title((main="Variable Missing Value Correlation"),sub = "Notice Whether variables are missing in sets")
  })
  
  ##############################################################EDA Numeric
  
  output$numeric_data_summary1 <- renderPrint({
    str(df_numeric)
  })
  
  output$numeric_data_summary2 <- renderPrint({
    summary(df_numeric)
  })
  
  output$numeric_data <- DT::renderDataTable({
    DT::datatable(df_numeric)
  })
  
  
  output$Histogram <- renderPlot({
    
    if (is.data.frame(df_numeric) && ncol(df_numeric) > 0) {
      num_columns <- ncol(df_numeric)
      
      
      par(mfrow = c(4, 3)) 
      
      
      colors <- rainbow(num_columns)  
      

      for (i in 1:num_columns) {
        col_name <- colnames(df_numeric)[i]  
        hist(df_numeric[[col_name]],  
             main = paste("Histogram of", col_name),  
             xlab = col_name,  
             ylab = "Frequency", 
             col = colors[i],  
             border = "black", 
             breaks = 30  
        )
      }
      
      
      par(mfrow = c(1, 1))
    } else {
      plot.new()  
      title("No Data Available") 
    }
  })
    
 
  
  output$Boxplot <- renderPlot({
    data_box <- as.matrix(df_numeric)
    data_box <- scale(data_box, center = input$standardise, scale = input$standardise)
    car::Boxplot(y = data_box, ylab = NA, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
                 horizontal = FALSE, outline = input$outliers, 
                 col = brewer.pal(n = dim(df_numeric)[2], name = "RdBu"),
                 range = input$range, main = "Boxplots of Numeric data", 
                 id = ifelse(input$outliers, list(n = Inf, location = "avoid"), FALSE)) 
    
  })
  
  
  output$Corrgram <- renderPlot({
    corrgram(df_numeric, 
             order = input$Group, 
             abs = input$abs, 
             cor.method = input$CorrMeth,
             text.panel = panel.txt,
             main = "Correlation of Numeric data")
  })
  

  output$Pairs <- renderPlot({

    GGally::ggpairs(data = df_numeric,
                    mapping = ggplot2::aes(color = "pink"),title = "Pairs plot of numeric data")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  #######################################################################################################
  output$catogoric_data_summary1 <- renderPrint({
    str(df_factor )
  })
  
  output$catogoric_data_summary2 <- renderPrint({
    summary(df_factor )
  })
  output$catogoric_data <- DT::renderDataTable({
    DT::datatable(df_factor)
  })
  
  output$Mosaic <- renderPlot({
    formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
    vcd::mosaic(formula, data = df_factor,
                main = "Mosaic Plot", shade = TRUE, legend = TRUE)
  })
  
  
  
  
  #######################################################
  
  output$originalData <- renderDT({
    datatable(processedData())
  })
  
  
  filteredData <- eventReactive(input$applyThreshold, {
    threshold_var <- input$varThreshold
    threshold_obs <- input$obsThreshold
    
    
    missing_percent_vars <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
    vars_to_keep <- names(missing_percent_vars[missing_percent_vars <= threshold_var])
    data_filtered_vars <- data[, vars_to_keep]
    
    
    obs_missing_percent <- apply(data_filtered_vars, 1, function(x) sum(is.na(x)) / length(x) * 100)
    obs_to_keep <- obs_missing_percent <= threshold_obs
    data_filtered_both <- data_filtered_vars[obs_to_keep, ]
    
    return(data_filtered_both)
  })
  output$Filtered_data_summary1 <- renderPrint({
    str(filteredData())
  })
  
  
  output$Filtered_data_summary2 <- renderPrint({
    summary(filteredData())
  })
  
  
  output$filteredData <- renderDT({
    datatable(filteredData())
  })
  
  output$missing_plot2 <- renderPlot({
    
    missing_percentages2 <- colSums(is.na(filteredData())) / nrow(filteredData()) * 100
    missing_data_df2 <- data.frame(Variable = names(missing_percentages2), MissingPercentage = missing_percentages2)
    
    
    ggplot(missing_data_df2, aes(x = reorder(Variable, MissingPercentage), y = MissingPercentage, fill = MissingPercentage)) +
      geom_bar(stat = "identity") +
      coord_flip() + 
      scale_fill_gradient(low = "green", high = "red") + 
      labs(x = "Variable", y = "Percentage of Missing Data", title = "Missing Data by Variable")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$upset_plot2 <- renderPlot({
    gg_miss_upset(filteredData(),nsets=7)
  })
  output$vis_miss_plot2 <- renderPlot({
    vis_miss(filteredData())
  })
  output$vis_dat_plot2 <- renderPlot({
    vis_dat(filteredData())
  }, res = 100)
  
 ##############################################
  
  
  filteredData2 <- eventReactive(input$applyThreshold2, {
    data_wo_code2 <- data  
    
    vars_to_keep2 <- names(which(colMeans(!is.na(data_wo_code2)) >= (100 - input$varThreshold2) / 100))
    obs_to_keep2 <- which(rowMeans(!is.na(data_wo_code2[vars_to_keep2])) >= (100 - input$obsThreshold2) / 100)
    
    data_filtered2 <- data_wo_code2[obs_to_keep2, vars_to_keep2]
    return(data_filtered2)
  })
  
  p_data <- reactive({
    rec <- recipe(DEATH_RATE ~ ., data = filteredData2()) %>%
      update_role("CODE", new_role = "id") %>%
      update_role("OBS_TYPE", new_role = "split")
    
    if (input$imputeMethod == "KNN Imputation") {
      rec <- rec %>% step_impute_knn(all_predictors(), neighbors = input$neighbors)
    } else if (input$imputeMethod == "Median Imputation") {
      rec <- rec %>% step_impute_median(all_numeric_predictors())
    } else if (input$imputeMethod == "Mean Imputation") {
      rec <- rec %>% step_impute_mean(all_numeric_predictors())
    }
    
    if (input$scale) {
      rec <- rec %>% step_scale(all_numeric_predictors())
    }
    
    if (input$center) {
      rec <- rec %>% step_center(all_numeric_predictors())
    }
    
    rec <- rec %>% step_dummy(all_nominal_predictors(), -all_outcomes())
    
    prep(rec)
    
  
  })
  
  train_control <- trainControl(method = "cv", number = 10)
  
  trained_model <- reactive({
    train(p_data(), data = train, method = "glmnet", trControl = train_control)
  })
  
  
  predictions_test <- reactive({
    predict(trained_model(), newdata = test)
  })
  
  predictions_train <- reactive({
    predict(trained_model(), newdata = train)
  })
  
  
  rmse_test <- reactive({
    sqrt(mean((test$DEATH_RATE - predictions_test())^2))
  })
  
  output$rmseOutput <- renderText({
    paste("Test RMSE:", format(rmse_test(), nsmall = 3))
  })
  
  output$predictions <- renderTable({
    test_predictions <- predict(trained_model(), newdata = test)
    data.frame( Actual_DEATH_RATE = test$DEATH_RATE, Predicted_DEATH_RATE = test_predictions)
  })
  
  output$predictionPlot <- renderPlot({
    test_predictions <- predict(trained_model(), newdata = test)
    max_val <- max(max(test$DEATH_RATE, na.rm = TRUE), max(test_predictions, na.rm = TRUE))
    ggplot(data = data.frame(Actual = test$DEATH_RATE, Predicted = test_predictions), aes(x = Actual, y = Predicted)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = 'red', linetype = "dashed") +
      ggtitle("Actual vs. Predicted Death Rates") +
      xlab("Actual Death Rate") +
      ylab("Predicted Death Rate") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))+
      xlim(0, max_val) + 
      ylim(0, max_val) +
      coord_fixed(ratio = 1)
  })
  
  output$residualPlotScatter <- renderPlot({
    residuals <- test$DEATH_RATE - predictions_test()
    ggplot(data.frame(Predicted = predictions_test(), Residuals = residuals), aes(x = Predicted, y = Residuals)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Residuals vs. Predicted Death Rate", x = "Predicted Death Rate", y = "Residuals")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  
  output$residualPlot <- renderPlot({
    train_residuals<-test$DEATH_RATE - predictions_test()
    test_residuals<-train$DEATH_RATE - predictions_train()
    
    
    residuals <- c(train_residuals, test_residuals)
    dataset <- factor(c(rep("Train", length(train_residuals)), rep("Test", length(test_residuals))))
    
    
    lower_bound <- quantile(residuals, probs = 0.25) - (IQR(residuals) * input$iqrMultiplier)
    upper_bound <- quantile(residuals, probs = 0.75) + (IQR(residuals) * input$iqrMultiplier)
   
    
    residuals_df <- data.frame(Residuals = residuals, Dataset = dataset)
    
    ggplot(residuals_df, aes(x = Dataset, y = Residuals)) +
      geom_boxplot() +
      geom_hline(yintercept = c(lower_bound, upper_bound), col = "red", linetype = "dashed") +
      geom_label(data = subset(residuals_df, Residuals < lower_bound | Residuals > upper_bound),
                 aes(label = ifelse(Dataset == "Train", paste("Train", row.names(residuals_df)[Residuals < lower_bound | Residuals > upper_bound]), 
                                    paste("Test", row.names(residuals_df)[Residuals < lower_bound | Residuals > upper_bound]))), 
                 position = position_dodge(width = 0.75), hjust = 1.5, vjust = -0.5) +
      labs(title = "Residuals Box-Plot", x = "", y = "Residuals") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  

  
  
  ################################################################################################
  filteredData3 <- eventReactive(input$applyThreshold3, {
    data_wo_code3 <- data  
    
    
    vars_to_keep3 <- names(which(colMeans(!is.na(data_wo_code3)) >= (100 - input$varThreshold3) / 100))
    
    
    obs_to_keep3 <- which(rowMeans(!is.na(data_wo_code3[vars_to_keep3])) >= (100 - input$obsThreshold3) / 100)
    
    
    data_filtered3 <- data_wo_code3[obs_to_keep3, vars_to_keep3]
    
    
    return(data_filtered3) 
  })
  
  p_data_partial <- reactive({
  
    partial_data <- na.omit(filteredData3())
    
    rec <- recipe(DEATH_RATE ~ ., data = partial_data) %>%
      update_role("CODE", new_role = "id") %>%
      step_naomit(all_predictors()) %>%  # omit NA values
      update_role("OBS_TYPE", new_role = "split")
    
    
    if (input$scale3) {
      rec <- rec %>% step_scale(all_numeric_predictors())
    }
    
    if (input$center3) {
      rec <- rec %>% step_center(all_numeric_predictors())
    }
    
    
    rec <- rec %>% step_dummy(all_nominal_predictors(), -all_outcomes())
    
    prep(rec) 
  })
  
  train_control1 <- trainControl(method = "cv", number = 10)
  
  trained_model1 <- reactive({
    partial_data <- na.omit(filteredData3())
    train_partial <- partial_data[partial_data$OBS_TYPE == "Train", ]
    train(DEATH_RATE ~ ., data = train_partial, method = "glmnet", trControl = train_control1)
  })
  
  predictions_test1 <- reactive({
    partial_data <- na.omit(filteredData3())
    test_partial <- partial_data[partial_data$OBS_TYPE == "Test", ]
    predict(trained_model1(), newdata = test_partial)
  })
  predictions_train1 <- reactive({
    partial_data <- na.omit(filteredData3())
    train_partial <- partial_data[partial_data$OBS_TYPE == "Train", ]
    predict(trained_model1(), newdata = train_partial)
  })
  
  
  rmse_test1 <- reactive({
    partial_data <- na.omit(filteredData3())
    test_partial <- partial_data[partial_data$OBS_TYPE == "Test", ]
    
    if (nrow(test_partial) > 0) {
      
      sqrt(mean((test_partial$DEATH_RATE - predictions_test1())^2, na.rm = TRUE))
    } else {
      NA  
    }
  })
  
  
  output$rmseOutput2 <- renderText({
    rmse <- rmse_test1()
    if (is.na(rmse)) {
      
    } else {
      paste("Test RMSE:", format(rmse, nsmall = 3))
    }
  })
  
  output$predictions2 <- renderTable({
    partial_data <- na.omit(filteredData3())
    test_partial <- partial_data[partial_data$OBS_TYPE == "Test", ]
    data.frame(OBS_TYPE = test_partial$OBS_TYPE, Actual_DEATH_RATE = test_partial$DEATH_RATE, Predicted_DEATH_RATE = predictions_test1())
  })
  
  output$partial <- renderDT({
    partial_data <- na.omit(filteredData3())
    datatable(partial_data)
  })
  
  output$Partially_summary1 <- renderPrint({
    partial_data <- na.omit(filteredData3())
    str(partial_data)
  })
  
  output$Partially_summary2 <- renderPrint({
    partial_data <- na.omit(filteredData3())
    summary(partial_data)
  })
  
  output$predictionPlot2 <- renderPlot({
    partial_data <- na.omit(filteredData3())
    test_partial <- partial_data[partial_data$OBS_TYPE == "Test", ]
    test_predictions <- predict(trained_model1(), newdata = test_partial)
    max_val <- max(max(test_partial$DEATH_RATE, na.rm = TRUE), max(test_predictions, na.rm = TRUE))
    ggplot(data = data.frame(Actual = test_partial$DEATH_RATE, Predicted = test_predictions), aes(x = Actual, y = Predicted)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = 'red', linetype = "dashed") +
      ggtitle("Actual vs. Predicted Death Rates") +
      xlab("Actual Death Rate") +
      ylab("Predicted Death Rate") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))+
      xlim(0, max_val) + 
      ylim(0, max_val) +
      coord_fixed(ratio = 1)
  })
  
  output$residualPlotScatter2 <- renderPlot({
    partial_data <- na.omit(filteredData3())
    test_partial <- partial_data[partial_data$OBS_TYPE == "Test", ]
    residuals <- test_partial$DEATH_RATE - predictions_test1()
    ggplot(data.frame(Predicted = predictions_test1(), Residuals = residuals), aes(x = Predicted, y = Residuals)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Residuals vs. Predicted Death Rate", x = "Predicted Death Rate", y = "Residuals")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$residualPlot2 <- renderPlot({
    partial_data <- na.omit(filteredData3())
    test_partial <- partial_data[partial_data$OBS_TYPE == "Test", ]
    train_partial <- partial_data[partial_data$OBS_TYPE == "Train", ]
    
    train_residuals<-train_partial$DEATH_RATE - predictions_train1()
    test_residuals<-test_partial$DEATH_RATE - predictions_test1()
    
    
    residuals <- c(train_residuals, test_residuals)
    dataset <- factor(c(rep("Train", length(train_residuals)), rep("Test", length(test_residuals))))
    
    
    lower_bound <- quantile(residuals, probs = 0.25) - (IQR(residuals) * input$iqrMultiplier3)
    upper_bound <- quantile(residuals, probs = 0.75) + (IQR(residuals) * input$iqrMultiplier3)
    
    
    residuals_df <- data.frame(Residuals = residuals, Dataset = dataset)
    
    ggplot(residuals_df, aes(x = Dataset, y = Residuals)) +
      geom_boxplot() +
      geom_hline(yintercept = c(lower_bound, upper_bound), col = "red", linetype = "dashed") +
      geom_label(data = subset(residuals_df, Residuals < lower_bound | Residuals > upper_bound),
                 aes(label = ifelse(Dataset == "Train", paste("Train", row.names(residuals_df)[Residuals < lower_bound | Residuals > upper_bound]), 
                                    paste("Test", row.names(residuals_df)[Residuals < lower_bound | Residuals > upper_bound]))), 
                 position = position_dodge(width = 0.75), hjust = 1.5, vjust = -0.5) +
      labs(title = "Residuals Box-Plot", x = "", y = "Residuals") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  
  #############################################################################################
  

  

}