# installing packages
#install.packages('gvis')
# importing packages
library(ggvis)
library(tidyverse)
library(class)

# the 'iris' dataset is an often used sample dataset for machine learning,
# especially classification. It contains 50 samples each of three iris varieties
# and we can use data on petal and sepal size to classify the variety

# familiarizing yourself with the data is an important step to understand
# which features might inform our classification task, look at summary 
# statistics, plot data, etc.

# load and inspect the data
df <- iris
summary(df)

# pairwise scatterplots
df %>%
  ggvis(~Sepal.Length, ~Sepal.Width, fill= ~Species) %>% 
  layer_points()

df %>%
  ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% 
  layer_points()

# Two important steps before we run any model:
# 1. feature engineering (e.g. normalization, standardization)
# 2. Train-Test Split

## 1. Feature engineering
### normalization:

norm <- function(col){
  colmax <- max(col)
  colmin <-min(col)
  
  result <- (col-colmin)/(colmax-colmin)
  result
}

df$sl_norm <- norm(df$Sepal.Length)
df$sw_norm <- norm(df$Sepal.Width)
df$pl_norm <- norm(df$Petal.Length)
df$pw_norm <- norm(df$Petal.Width)

summary(df)

## 2. Train-Test Split
# we need to make two things sure: we need to have enough training and test data,
# the split should favor the training size to get a more accurate model, so 50/50 is
# not a good idea, maybe 67% training and 33% test. And second, the samples have
# to be samples, we don't want to learn only from two varieties and then predict
# the third, so shuffle your data :)

# set the random state seed, it can be whatever you want
set.seed(1337)

# create partition for the samples
split <- sample(2, nrow(df), replace=TRUE, prob=c(.67, .33))
df$split <- as.factor(split)

# training and test sets (with labels)
train <- df %>%
  dplyr::filter(split==1) 
test <- df %>% 
  dplyr::filter(split==2) 

# extract labels
y_train <- as.factor(train$Species)
y_test <- as.factor(test$Species)
  
# training and test set (without labels)
# why without labels?
x_train <- train %>% 
  select(sl_norm, sw_norm, pl_norm, pw_norm)
x_test <- test %>% 
  select(sl_norm, sw_norm, pl_norm, pw_norm)

# running the knn-algorithm with training and test data and separate 
# training labels
pred <- class::knn(x_train, x_test, y_train, k=3)

# evaluating the model: comparing prediction against the real labels
tab <- table(pred, y_test)
tab

# accuracy (multiclass): correct classifications / all classifications
correct = 20 + 19 + 15
all = correct + 1

accuracy <- correct/all
accuracy

# finally we have to think about over- and underfitting:

## overfitting: the model is extremely good at predicting test values but can't 
## generalize, especially a problem in non-parametric models
## -> reducing the learning of details

## underfitting: the model is bad at learning the test data AND can't generalize

### at-home task: repeat everything with non-normalized values and compare the results!