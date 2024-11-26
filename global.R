library(shiny)
library(DT)
library(VIM)
library(GGally)
library(readr)
library(ggplot2)
library(vcd)
library(dplyr)
library(corrplot)
library(tidyr)
library(devtools)
library(tabplot)
library(naniar)
library(visdat)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(corrgram)
library(recipes)
#library(tidymodels)
library(caret)
library(glmnet)
library(DataExplorer)




Ass2Data <- read.csv("Ass2Data.csv", header = TRUE, na.strings = c("","NA", "na", "N/A", "-1", "-99","--"),stringsAsFactors = TRUE)

#Ass2Data is the data set just after the placeholders without doing anything... 

Ass2Data_pairs<-Ass2Data%>%select(-CODE)

#Ass2Data_pairs is the data table for pairs plot for raw data without the CODE variable

data<- Ass2Data

#data$POPULATION<-as.numeric(as.factor(data$POPULATION))
#data$AGE25_PROPTN<-as.numeric(as.factor(data$AGE25_PROPTN))
#data$AGE_MEDIAN<-as.numeric(as.factor(data$AGE_MEDIAN))
#data$AGE50_PROPTN <-as.numeric(as.factor(data$AGE50_PROPTN ))
#data$POP_DENSITY<-as.numeric(as.factor(data$POP_DENSITY))
#data$INFANT_MORT <-as.numeric(as.factor(data$INFANT_MORT))
#data$GDP  <-as.numeric(as.factor(data$GDP))
#data$DOCS  <-as.numeric(as.factor(data$DOCS))
#data$VAX_RATE <-as.numeric(as.factor(data$VAX_RATE))
#data$HEALTHCARE_COST  <-as.numeric(as.factor(data$HEALTHCARE_COST))

for (col_name in names(data)) {
  if (is.numeric(data[[col_name]])) {
    data[[col_name]][data[[col_name]] == -1] <- NA
  }
}

for (col_name in names(data)) {
  if (is.numeric(data[[col_name]])) {
    data[[col_name]][data[[col_name]] == -99] <- NA
  }
}

#I was not able to find any scenario, where the "Not Applicable" missingness in the categorical variables.

# But, When skimming trough the data set, it is found that when the HEALTHCARE_BASIS is FREE, the value of the HEALTHCARE_COST is "NA".
# This is a "Not Applicable" scenario in this data set. 

#create a shadow variable
data$HEALTHCARE_COST_shadow<-   as.numeric(is.na(data$HEALTHCARE_COST))
# Assign missing to zero
data$HEALTHCARE_COST[is.na(data$HEALTHCARE_COST)]  <-  0
data <- data %>% select(-HEALTHCARE_COST_shadow)


corr_data <-is.na(data)+0
cm <- colMeans(corr_data)
corr_data<- as.data.frame(corr_data[,cm >0 & cm <1, drop= FALSE])
corr_data1 <- reactive({
  corr_data
  return(corr_data)
  })

df_numeric <- data[sapply(data, is.numeric)]
df_factor <- data[sapply(data, is.factor)]
df_factor <-df_factor[-1]
choicesA <- setdiff(colnames(df_factor), "OBS_TYPE")

for (col_name in names(data)) {
  if (is.factor(data[[col_name]])) {
    data[[col_name]] <- as.character(data[[col_name]])
    data[[col_name]][is.na(data[[col_name]]) | data[[col_name]] == "NA"] <- "none"
    data[[col_name]] <- as.factor(data[[col_name]])
  }
}



train <- data[data$OBS_TYPE == "Train", ]
test <- data[data$OBS_TYPE == "Test", ]


