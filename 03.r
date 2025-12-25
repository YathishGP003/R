# 3. Program to implement Linear regression for a given dataset.
# Dataset: Marketing
# Source of the Dataset: “datarium” Package
# Objective: To predicting the sales units, given the amount of money spent in the advertising media
# named youtube.
# Program:

#install.packages("caret")
library(dplyr) ## For data manipulation and visualization
library(ggplot2) ## For plots
library(caret) ## For createDataPartition function
library(datarium) ## For Marketing dataset

data("marketing", package = "datarium")

#Exploratory Data Analysis
head(marketing)

## youtube facebook newspaper sales
## 1 276.12 45.36 83.04 26.52
## 2 53.40 47.16 54.12 12.48
## 3 20.64 55.08 83.16 11.16
## 4 181.80 49.56 70.20 22.20
## 5 216.96 12.96 70.08 15.48
## 6 10.44 58.68 90.00 8.64

#dimension of the dataset
dim(marketing)

## [1] 200 4

#Overview of the dataset
summary(marketing)

## youtube facebook newspaper sales
## Min. : 0.84 Min. : 0.00 Min. : 0.36 Min. : 1.92
## 1st Qu.: 89.25 1st Qu.:11.97 1st Qu.: 15.30 1st Qu.:12.45
## Median :179.70 Median :27.48 Median : 30.90 Median :15.48
## Mean :176.45 Mean :27.92 Mean : 36.66 Mean :16.83
## 3rd Qu.:262.59 3rd Qu.:43.83 3rd Qu.: 54.12 3rd Qu.:20.88
## Max. :355.68 Max. :59.52 Max. :136.80 Max. :32.40

#Structure of the dataset
str(marketing)

## 'data.frame': 200 obs. of 4 variables:
## $ youtube : num 276.1 53.4 20.6 181.8 217 ...
## $ facebook : num 45.4 47.2 55.1 49.6 13 ...
## $ newspaper: num 83 54.1 83.2 70.2 70.1 ...
## $ sales : num 26.5 12.5 11.2 22.2 15.5 ...

# Checking missing cases
sum(!complete.cases(marketing))

## [1] 0

##Looking at Sales attribute
ggplot(marketing, aes(sales)) +
    geom_histogram(binwidth = 5) +
    labs(title = "Histogram of Sales", y = "Count") +
    theme_classic()

# Visualize the data (displaying the sales in thousands of dollars vs. Youtube advertising budget
ggplot(marketing, aes(x = youtube, y = sales)) +
    geom_point() +
    stat_smooth()

## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

#Correlation coefficient
#Measures the level of association between X and Y
#~0: weak relationship between the variables

cor(marketing$sales, marketing$youtube)

## [1] 0.7822244
# Split the data into training and test set
# Here seed is set to ensure reproducibility; Value for seed can any positive integer value

set.seed(123)
training.samples <- marketing$sales %>% createDataPartition(p = 0.8, list = FALSE)

train.data <- marketing[training.samples,]
test.data <- marketing[-training.samples,]

#Simple Linear Regression; First Parameter represent Formula and Second Parameter represent data
model <- lm(sales ~ youtube, data = train.data)

#Summarize the model
summary(model)$coef

## Estimate Std. Error t value Pr(>|t|)
## (Intercept) 8.58914961 0.616044182 13.94242 1.987874e-29
## youtube 0.04671639 0.003003398 15.55451 8.019035e-34
#The output above shows the estimate of the regression beta coefficients (column Estimate) and their significance
#levels (column Pr(>|t|)). The intercept (b0) is 8.38 and the coefficient of youtube variable is 0.046.
#The estimated regression equation can be written as:sales = 8.38 + 0.046*youtube.

#Predictions can be easily made using the R function predict().
#Make Predictions using Test data
predictions <- model %>% predict(test.data)

# Model Performance can be measured using any one of the following metrics

#(a) Compute the prediction error, RMSE; RMSE value must be lower for the better model
RMSE(predictions, test.data$sales)

## [1] 3.687985

#(b) Compute R-Square; R-Square value must be near to 1 for the better model
R2(predictions, test.data$sales)

## [1] 0.6582824

#In the following example, we predict sales units for two youtube advertising budget: 0 and 1000.

# Making Predictions for new data
newdata <- data.frame(youtube=c(0,1000))
predictions <- model %>% predict(newdata)

predictions
## [1] 8.58915
## [2] 55.30554
