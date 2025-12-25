# 2. Program to perform Data exploration and Pre-processing on a given dataset.
# Dataset: Cars (cars_multi.csv & cars_price.csv)
# URL for the Dataset: Click here
# Objective: To understand how the attributes / columns / features in the dataset relate to each
# other, to uncover interesting things, and to communicate those findings.
# Assumption: The Cars dataset i.e., CSV files are present in the current working directory
# Program:

#if the library has not been installed then you can install the library using the below syntax [Here: corrplot is the library]
#install.packages("corrplot")
# Importing library
library(dplyr) ## For data manipulation and visualization
library(ggplot2) # For plots
library(corrplot) ##provides a visual exploratory tool on correlation matrix
# Load data
cars_multi <- read.csv("cars_multi.csv")
cars_price <- read.csv("cars_price.csv")
#Exploratory Data Analysis
# Head of the dataset cars_multi
head(cars_multi)

head(cars_price)

dim(cars_multi)

dim(cars_price)

# Join two dataset
cars <- left_join(cars_multi, cars_price, by = "ID")
#Display names of columns / attributes / features / variables
colnames(cars)

# Checking missing cases
sum(!complete.cases(cars))
## [1] 0
#Overview of the dataset
summary(cars)

#Structure of the dataset
str(cars)


#Looking at each variable / attribute / feature
#1. MPG: Mpg means Miles per gallon and we want to know the most common value
ggplot(cars, aes(mpg)) +
    geom_histogram(binwidth = 5) +
    labs(title = "Histogram of MPG", y = "Count") +
    theme_classic()

#Finding: We can see that the most common mpg is something between 15 and 20 mpg
#2. Cylinders
ggplot(cars, aes(cylinders)) +
    geom_bar() +
    labs(title = "Cylinders", y = "Count") +
    theme_classic()
    
#Finding: For cylinders we can see that 4 cylinders is 2 times more often than 8 cylinders
#3. Displacement
boxplot(cars$displacement, data=cars$displacement, main="Box Plot Displacement", xlab="", ylab="Displacement")

#Finding: There is more data/value greater than the average
#4: Horsepower
count(cars[as.character(cars$horsepower) == "?",])

#Finding: In fact we have 6 missing values at horsepower.
#5: Weight
ggplot(cars, aes(weight)) +
    geom_histogram(binwidth = 5) +
    labs(title = "Histogram of Weight", y = "Count") +
    theme_classic()
    
#Finding: For Weight we see that the most common weight is something between 2000 and 3000. But most important we saw that we have the only one unique weight for the majority of the cars
#6: Acceleration
ggplot(cars, aes(acceleration)) +
    geom_density() +
    labs(title = "Density of Weight") +
    theme_classic()
    
#Finding: We see that the density of acceleration is more concentrate at 15
#7. Model
to_Plot <- as.data.frame(table(cars$model))
colnames(to_Plot) <- c("Model", "Frequency")

ggplot(to_Plot, aes(x = Model, y = Frequency)) +
    geom_bar(stat = "identity") +
    labs(title = "Model") +
    theme_classic()
    
#Finding: As we can see we have a good a balance sample for model
#8. Origin
ggplot(cars, aes(origin)) +
    geom_bar() +
    labs(title = "Origin", y = "Count") +
    theme_classic()
    
#Finding: We have the majority of the cars from origin 1
#9. Price
#For price we decide to do some pre-processing to make more simpler. We are going to keep only the value before the point. For example: If we have 1598.07337 we are going to keep only 1598.
#We made this decision because we believe that the value after the point is meanin gless
cars$price <- as.integer(cars$price)
boxplot(cars$price, data=cars$p

#Finding: Looking at the box plot of price we can see that we have only one Outlier
to_Plot <- as.data.frame(table(cars$price))
colnames(to_Plot) <- c("Price", "Frequency")

ggplot(head(to_Plot[ order(-to_Plot[,2]), ]), aes(x = reorder(Price, Frequency), y
= Frequency)) +
    geom_bar(stat = "identity") +
    labs(title = "Common Price", x = "Price") +
    theme_classic() +
    coord_flip()
    
#Finding: With this visualization we can see that we have 3 price that repeat more than 40 times. We have 219 unique prices. This could be a problem if we have to predict the price of the cars because we have unbalanced data
#Correlation
#At this plot we can see the correlation between all features.
# Transforming from factor to numeric

cars$horsepower <- as.numeric(as.character(cars$horsepower))
## Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion
# Removing not complete row
cars <- cars[complete.cases(cars),]

# Removing the ID
cars <- cars[,-1]
nums <- sapply(cars, is.numeric)
correlations <- cor(cars[,nums])
corrplot(correlations, order = "hclust")
