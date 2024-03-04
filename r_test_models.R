
#loading dataset
library(readr)

data <- read_csv("E:\\r_miniproject\\bank-full_train.csv")



#colnames to drop
colnames(data)
#checking duplicates in "ID"
sum(duplicated(data$ID))
remove_cols <- c("ID")
#remove columns in list 
data <- subset(data, select = !(names(data) %in% remove_cols))
## Check if any imbalanced classes in dataset


#library(tidymodels)
library(ggplot2)
# Plot job occupation against the target variable
#checking for class imbalance
ggplot(data, aes(job, fill = y)) +
  geom_bar() +
  coord_flip()


library(ggplot2)
# Plot job occupation against the target variable
#checking for class imbalance
ggplot(data, aes( default)) +
  geom_bar() +
  coord_flip()
table(data$default)
#data <- subset(data, select = -default)

#library(cleandata)
#encoding ordinal data

#data$poutcome <- as.numeric(factor(data$poutcome,order = TRUE,
#                                   levels = c('failure','unknown','other','success')))




#converting yes , no to numeric data
library(plyr)


#y
data$y  <- revalue(data$y, c("yes"=1))
data$y <- revalue(data$y, c("no"=0))
data$y <- as.numeric(data$y)
head(data$y)
data$y = as.factor(data$y)

head(data$y)

# default
# data$default  <- revalue(data$default, c("yes"=1))
# data$default <- revalue(data$default, c("no"=0))
# data$default <- as.numeric(data$default)
# head(data$default)
########################
#encoding nominal data
#education data

# Load the library
library(fastDummies)
unique(data$education)

# Create dummy variable

data <- dummy_cols(data,select_columns = "month")
data <- subset(data, select = c(-month))

data <- dummy_cols(data,select_columns = "poutcome")
data <- subset(data, select = c(-poutcome))

data <- dummy_cols(data,select_columns = "marital")
data <- subset(data, select = c(-marital))

data <- dummy_cols(data,select_columns = "job")
data <- subset(data, select = c(-job))

data <- dummy_cols(data,select_columns = "contact")
data <- subset(data, select = c(-contact))

data <- dummy_cols(data,select_columns = "housing")
data <- subset(data, select = c(-housing,-housing_yes))

data <- dummy_cols(data,select_columns = "loan")
data <- subset(data, select = c(-loan,-loan_yes))

data <- dummy_cols(data,select_columns = "default")
data <- subset(data, select = c(-default,-default_yes))

data <- dummy_cols(data,select_columns = "education")
data <- subset(data, select = c(-education))
###renaming colname ::: Error in eval(predvars, data, env) : object 'job_blue-collar' not found
names(data)[names(data)=="job_blue-collar"] <- "job_bluecollar"

names(data)[names(data)=="job_self-employed"] <- "job_selfemployedr"


##################################################################################################################################################






#variance in data

var(data$balance)

#Unique jobs
uni <- unique(data$job)


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



# Create confusion matrix
conf_mat(results, truth = y,
         estimate = .pred_class)
# variables impacting the subscription buying decision.
coeff <- tidy(model_lr) %>%
  arrange(desc(abs(estimate))) %>%
  filter(abs(estimate) > 0.5)
#####################################################################
#random forest 

library(randomForest)


#fit the random forest model
model <- randomForest(
  formula = y ~ .,
  data = train
)


summary(model)

randomForest::varImpPlot(model,
                         sort=FALSE,
                         main="Variable Importance Plot")

pred <- predict(model,test, type="class")
test$predicted_y<-pred
library('caret')
confusionMatrix(pred, test$y)
############analysing result
##############################################
will_subscribe <- sum(test$predicted_y == 1)
will_not_subscribe <-sum(test$predicted_y == 0)

x <- c(will_subscribe,will_not_subscribe)
labels <- c("will subscribe ", "will not subscribe")



# Plot the chart with title and rainbow
# color pallet.
pie(x, labels, main = "Analysis",
    col = rainbow(length(x)))