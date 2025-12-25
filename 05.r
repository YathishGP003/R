# 5. Program to implement K-NN algorithm on a given dataset.
# Dataset: credit_data
# URL for the Dataset: Click here
# Objective: To study a bank credit dataset and use the K-NN algorithm to classify the applicant’s
# loan request into two classes namely “Approved” and “Disapproved”.
# Assumption: The credit dataset i.e., CSV file are present in the current working directory
# Program:

#Import the dataset
loan <- read.csv("credit_data.csv")

#Structure
str(loan)

loan.subset <- loan[c('Creditability','Age..years.','Sex...Marital.Status','Occupa
tion','Account.Balance','Credit.Amount','Length.of.current.employment','Purpose')]

#Again see the structure
#Now we have narrowed down 21 variables to 8 predictor variables that are significant for building the model.
str(loan.subset)

#Data Normalization
#You must always normalize the data set so that the output remains unbiased. To explain this, let's take a look at the first few observations in our data set.
head(loan.subset)

#Notice the Credit amount variable, its value scale is in 1000s, whereas the rest of the variables are in single digits or 2 digits. If the data isn't normalized it will lead to a biased outcome.
#Normalization function
normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x))) }

#In the below code snippet, we're storing the normalized data set in the 'loan.sub set.n' variable and
#also we're removing the 'Credibility' variable since it's the response variable that needs to be predicted.
loan.subset.n <- as.data.frame(lapply(loan.subset[,2:8], normalize))
#Normalized dataset
head(loan.subset.n)

#Data Spliting
set.seed(123)
dat.d <- sample(1:nrow(loan.subset.n),size=nrow(loan.subset.n)*0.7,replace=FALSE)

#random selection of 70% data.
train.loan <- loan.subset[dat.d,] # 70% training data
test.loan <- loan.subset[-dat.d,] # remaining 30% test data
#Creating seperate dataframe for 'Creditability' feature which is our target.
train.loan_labels <- loan.subset[dat.d,1]
test.loan_labels <-loan.subset[-dat.d,1]

#Install class package
#install.packages('class')
# Load class package
library(class) # For K-NN

#Next, we're going to calculate the number of observations in the training data set.
#The reason we're doing this is that we want to initialise the value of 'K' in the K-NN model.
#One of the ways to find the optimal K value is to calculate the square root of the total number of
#observations in the data set. This square root will give you the 'K' value.

#Find the number of observations
NROW(train.loan_labels)

## [1] 700
#So, we have 700 observations in our training data set. The square root of 700 is around 26.45,
#therefore we'll create two models. One with 'K' value as 26 and the other model with a 'K' value as 27.
knn.26 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=26)
knn.27 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=27)

#Model Evaluation
#Calculate the proportion of correct classification for k = 26, 27
ACC.26 <- 100 * sum(test.loan_labels == knn.26)/NROW(test.loan_labels)
ACC.27 <- 100 * sum(test.loan_labels == knn.27)/NROW(test.loan_labels)

ACC.26

## [1] 68.66667
ACC.27

## [1] 69
#Finding: The accuracy for K = 26 is 68.66 and for K = 27 it is 69. We can also check the predicted outcome against the actual value in tabular form:
# Check prediction against actual value in tabular form for k=26
table(knn.26 ,test.loan_labels)

## test.loan_labels
## knn.26 0 1
## 0 8 7
## 1 87 198
knn.26

# Check prediction against actual value in tabular form for k = 27
table(knn.27 ,test.loan_labels)

## test.loan_labels
## knn.27 0 1
## 0 8 6
## 1 87 199
knn.27

#You can also use the confusion matrix to calculate the accuracy.
#To do this, we must first install the Caret package:
#install.packages('caret')

library(caret)
## Loading required package: ggplot2
## Loading required package: lattice

confusionMatrix(table(knn.26 ,test.loan_labels))

i=1
k.optm=1
for (i in 1:28){
  knn.mod <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=i)
  k.optm[i] <- 100 * sum(test.loan_labels == knn.mod)/NROW(test.loan_labels)
  k=i
  cat(k,'=',k.optm[i],'') }

#Accuracy plot; Graphical representation
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
