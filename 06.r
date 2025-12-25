# 6. Build a recommendation system using;
# i) Item Based Collaborative Filtering ii) User Based Collaborative Filtering
# Dataset: data
# URL for the Dataset: Click here
# Objective: To model item-based and user-based collaborative filtering recommendation.
# Assumption: The dataset i.e., CSV file are present in the current working directory
# Program:

#library(methods)
#library(knitr)
#The following libraries were used
library(recommenderlab) # To test and develop recommender algorithms
library(ggplot2)
library(data.table)

#Some pre-processing of the data available is required before creating the
recommendation system
df_data <- fread('data.csv')
df_data[ ,InvoiceDate := as.Date(InvoiceDate)]

#There is negative Quantity & Unit Price, also NULL/NA Customer ID. We will delete all the NA Row
df_data[Quantity<=0,Quantity:=NA]
df_data[UnitPrice<=0,UnitPrice:=NA]
df_data <- na.omit(df_data)

#Create a Item Dictionary which allows an easy search of a Item name by any of its StockCode
setkeyv(df_data, c('StockCode', 'Description'))
itemCode <- unique(df_data[, c('StockCode', 'Description')])
setkeyv(df_data, NULL)

#Convert from transactional to binary metrix, 0 for no transaction and vice versa
df_train_ori <- dcast(df_data, CustomerID ~ StockCode, value.var = 'Quantity',fun.aggregate = sum, fill=0)

CustomerId <- df_train_ori[,1] #!

df_train_ori <- df_train_ori[,-c(1,3504:3508)]

#Fill NA with 0
for (i in names(df_train_ori))
    df_train_ori[is.na(get(i)), (i):=0]
    
#In order to use the ratings data for building a recommendation engine with recommenderlab,
#we must convert buying matrix into a sparse matrix of type realRatingMatrix.
df_train <- as.matrix(df_train_ori)
df_train <- df_train[rowSums(df_train) > 5,colSums(df_train) > 5]
df_train <- binarize(as(df_train, "realRatingMatrix"), minRatin = 1)

#Training
#We will use Item Base Collaboratife Filtering or IBCF. Jaccard is used because our data is binary
#Dataset is split Randomly with 80% for training and 20% for test
which_train <- sample(x = c(TRUE, FALSE), size = nrow(df_train),replace = TRUE,
prob = c(0.8, 0.2))
y <- df_train[!which_train]
x <- df_train[which_train]

# For UBCF_binaryRatingMatrix
recommender_models<-recommenderRegistry$get_entries(dataType="binaryRatingMatrix")
recommender_models$IBCF_binaryRatingMatrix$parameters

#Training Dataset
method <- 'IBCF' #Another recommendation UBCF
parameter <- list(method = 'Jaccard')
n_recommended <- 5
n_training <- 1000
recc_model <- Recommender(data = x, method = method, parameter = parameter)
model_details <- getModel(recc_model)

#Predict
#Test Dataset is split randomly, We only use 20% for test.Return value of prediction is top-N-List of
#recommendation item for each user in test dataset.
recc_predicted <-predict(object = recc_model, newdata = y, n = n_recommended, type="topNList")

#Recomendation item for first 5 user in training dataset
as(recc_predicted,"list")[1:5]

# The Following Part will Change for UBCF
user_1 <- CustomerId[as.integer(names(recc_predicted@items[1]))]

#these are the recommendations for user_1
vvv <- recc_predicted@items[[1]]
vvv <- rownames(model_details$sim)[vvv]
itemCode[vvv]
