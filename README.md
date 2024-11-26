# Shiny-App --Outliers-Null-Values-Recipes
Summary
This assignment has two parts, coding a shiny app and producing a report.
Disclaimer: The data used in this assignment is not genuine; its has been artificially constructed to have
interesting characteristics and challenges embedded in it.
Part 1 - Coding
Create a Shiny app using RStudio. Load the supplied comma separated variables (csv) file and use Shiny to:
1. Summarise and visualise the data and perform Exploratory Data Analysis. Make good use of controls.
This is a minor part of the assignment so borrow from assignment 1 if you can.
2. From this evidence and the supplied background (see below) develop a strategy to deal with missing
values. The strategy can change for different variables.
3. From this evidence and the supplied background develop a strategy to deal with outliers. The strategy
can change for different variables.
4. Develop a pre-processing strategy for things like centring and scaling.
5. Implement these strategies using a “recipes” based data processing pipeline.
6. Develop a tuned glmnet model and visualise its test performance. Document the model’s optimal
hyper-parameters. Note that you DO NOT need to explore other methods for this assignment - just
glmnet.
7. Identify any residual outliers. Think about how to show the train and test residuals.
The submission should be a set of files: ui.R, server.R and global.R that we should be able to run and
grade (without needing to make any changes). Submit these files as a compressed ZIP file.
Part 2 - Report
Write a report on your modelling. Include appropriate images from your shiny app.
1. Discuss the data and any curious features that you noticed. Record the issues you would have followed
up with a domain expert, were one available.
2. Document and justify your various strategies using words (rather than code).
3. Research the glmnet method and briefly explain this method in your report.
4. Document your glmnet model’s theoretical performance on unseen data.

The Background
Covid-19 data, all measurements are as at 2019. The supplied CSV contains the following variables:
CODE Anonymised state or country
GOVERN_TYPE Type of government: "STABLE DEM", "UNSTABLE DEM", "DICTATORSHIP", "OTHER"
POPULATION Total population
AGE25_PROPTN The proportion of the population that is at or below 25
AGE_MEDIAN The median age of the population
AGE50_PROPTN The proportion of the population that is at or above 50
POP_DENSITY The population density
GDP The Gross National Product
INFANT_MORT The infant mortality rate
DOCS The number of doctors per 10,000
VAX_RATE The mean vaccination rate for Covid-19
HEALTHCARE_BASIS Type of healthcare system "INSURANCE", "PRIVATE", "FREE"
HEALTHCARE_COST Healthcare costs per person where applicable
DEATH_RATE The projected death rate (across ten years)
OBS_TYPE The allocation to test or train
The outcome variable is the DEATH_RATE.

