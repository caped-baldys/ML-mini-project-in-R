

#loading dataset
library(readr)

data <- read_csv("D:\\r_miniproject\\bank-full_train.csv")


##Visualize the Data
#library(tidymodels)
library(ggplot2)
# Plot job occupation against the target variable
#checking for class imbalance
ggplot(data, aes(job, fill = y)) +
  geom_bar() +
  coord_flip()








#view structure of data
str(data)

#view summary of dataset
summary(data)

#checking for null values in dataset
apply(data,2,function(x)sum(is.na(x)))

print(data[1:10])

max(data$day)

max(data$day)
#checking for useful categorical data
for (i in colnames(data[,sapply(data,is.character)])
)
{ print(i)
  print(unique(data[i]))
}


#colnames to drop
colnames(data)
#checking duplicates in "ID"
sum(duplicated(data$ID))
remove_cols <- c("ID")
#remove columns in list
data <- subset(data, select = !(names(data) %in% remove_cols))



#library(cleandata)
#encoding ordinal data

data$poutcome <- as.numeric(factor(data$poutcome,order = TRUE,
                                         levels = c('failure','unknown','other','success')))

data$education<- as.numeric(factor(data$education,order = TRUE,
                                        levels = c('unknown','primary','secondary','tertiary')))


data$month <- as.numeric(factor(data$month,order = TRUE,
                                         levels = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')))



#converting yes , no to numeric data
library(plyr)
#housing
data$housing  <- revalue(data$housing, c("yes"=1))
data$housing <- revalue(data$housing, c("no"=0))
data$housing <- as.numeric(data$housing)
head(data$housing)

#loan
data$loan  <- revalue(data$loan, c("yes"=1))
data$loan <- revalue(data$loan, c("no"=0))
data$loan <- as.numeric(data$loan)
head(data$loan)

#y
data$y  <- revalue(data$y, c("yes"=1))
data$y <- revalue(data$y, c("no"=0))
data$y <- as.numeric(data$y)
head(data$y)
data$y = as.factor(data$y)

head(data$y)

#default
data$default  <- revalue(data$default, c("yes"=1))
data$default <- revalue(data$default, c("no"=0))
data$default <- as.numeric(data$default)
head(data$default)
########################
#encoding nominal data
#education data

# Load the library
library(fastDummies)
unique(data$education)

# Create dummy variable
data <- dummy_cols(data,select_columns = "marital")
data <- subset(data, select = c(-marital,-marital_single))

data <- dummy_cols(data,select_columns = "job")
data <- subset(data, select = c(-job,-job_unknown))

data <- dummy_cols(data,select_columns = "contact")
data <- subset(data, select = c(-contact,-contact_unknown))

###renaming colname ::: Error in eval(predvars, data, env) : object 'job_blue-collar' not found
names(data)[names(data)=="job_blue-collar"] <- "job_bluecollar"

names(data)[names(data)=="job_self-employed"] <- "job_selfemployedr"


##################################################################################################################################################






#variance in data

var(data$balance)

#Unique jobs
uni <- unique(data$job)
#Plot function 
library(ggplot2)

data$idu <- as.numeric(row.names(data))
myplot <- function(x)
{
  G <- ggplot(data =subset(data,job==x),aes(x=idu,y=balance, colour = 'red'))+
    geom_point()+
    ggtitle(x)
  G
}
#Apply
List <- lapply(uni, myplot)

data <- subset(data, select = -idu)

###################################################################################################################
#Outlier
#checing if data$balance is normaly distributed or not
#hist(data$balance, col='steelblue', main='Balance')

#ks.test(data$balance, 'pnorm')







############################################################################################################################################


######spliting data into test and train

library(caTools)

#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample.split(data$age, SplitRatio = 0.7)
train  <- subset(data, sample == TRUE)
test   <- subset(data, sample == FALSE)



###################################################
##Train model

library(randomForest)


#fit the random forest model
model <- randomForest(
  formula = y ~ .,
  data = train
)

# model <- randomForest(
#   formula = y ~ .,
#   data = train,tree=100, keep.forest=FALSE,
#   importance=TRUE
# )

summary(model)

randomForest::varImpPlot(model,
                         sort=FALSE,
                         main="Variable Importance Plot")

pred <- predict(model,test, type="class")
library('caret')
confusionMatrix(pred, test$y)
#################################################################################
#logistic regression
library('tidymodels')
library('glmnet')
# Train a logistic regression model
model_lr <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(y ~ ., data = train)

# Class Predictions
pred <- predict(model_lr,test, type="class")

# Class Probabilities
pred_proba <- predict(model_lr,
                      new_data = test,
                      type = "prob")

## Evaluate the model performance on the testing set
results <- test %>%
  select(y) %>%
  bind_cols(pred, pred_proba)

accuracy(results, truth = y, estimate = .pred_class)
#######################################################################


results <- test %>%
  select(y) %>%
  bind_cols(pred_class, pred_proba)

# Create confusion matrix
conf_mat(results, truth = y,
         estimate = .pred_class)
# variables impacting the subscription buying decision.
coeff <- tidy(log_reg_final) %>% 
  arrange(desc(abs(estimate))) %>% 
  filter(abs(estimate) > 0.5)

