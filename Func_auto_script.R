

library(readr)
library(cleandata)

data_train <- read_csv("C:\\Users\\DELL\\Desktop\\Documents\\R_miniproject\\bank-full_train.csv")

remove_column <- function(df){
  ##since unique 
  remove_cols <- c("ID")
  #remove columns in list
  df <- subset(df, select = !(names(data_train) %in% remove_cols))
  return(df)
  
}

ordinal_encoding<-function(data_train){
  #encoding ordinal data
  
  data_train$poutcome <- as.numeric(factor(data_train$poutcome,order = TRUE,
                                           levels = c('failure','unknown','other','success')))
  
  data_train$education<- as.numeric(factor(data_train$education,order = TRUE,
                                           levels = c('unknown','primary','secondary','tertiary')))
  
  
  data_train$month <- as.numeric(factor(data_train$month,order = TRUE,
                                        levels = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')))
  
  return(data_train)
}

binary_encode<-function(data_train){
  
  #converting yes , no to numeric data
  library(plyr)
  #housing
  data_train$housing  <- revalue(data_train$housing, c("yes"=1))
  data_train$housing <- revalue(data_train$housing, c("no"=0))
  data_train$housing <- as.numeric(data_train$housing)
  head(data_train$housing)
  
  #loan
  data_train$loan  <- revalue(data_train$loan, c("yes"=1))
  data_train$loan <- revalue(data_train$loan, c("no"=0))
  data_train$loan <- as.numeric(data_train$loan)
  head(data_train$loan)
  
  #y
  data_train$y  <- revalue(data_train$y, c("yes"=1))
  data_train$y <- revalue(data_train$y, c("no"=0))
  data_train$y <- as.numeric(data_train$y)
  head(data_train$y)
  
  #default
  data_train$default  <- revalue(data_train$default, c("yes"=1))
  data_train$default <- revalue(data_train$default, c("no"=0))
  data_train$default <- as.numeric(data_train$default)
  head(data_train$default)
  
  return(data_train)
  
}


one_hot_encoding<-function(data_train){
  
  # Load the library
  library(fastDummies)
  unique(data_train$education)
  
  # Create dummy variable
  data_train <- dummy_cols(data_train,select_columns = "marital")
  data_train <- subset(data_train, select = c(-marital,-marital_single))
  
  data_train <- dummy_cols(data_train,select_columns = "job")
  data_train <- subset(data_train, select = c(-job,-job_unknown))
  
  data_train <- dummy_cols(data_train,select_columns = "contact")
  data_train <- subset(data_train, select = c(-contact,-contact_unknown))
  
  return(data_train)
  
}


data_train<-remove_column(data_train)

data_train<-ordinal_encoding(data_train)

data_train<-binary_encode(data_train)

data_train<-one_hot_encoding(data_train)

