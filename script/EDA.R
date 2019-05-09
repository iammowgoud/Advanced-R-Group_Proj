library(tidyr)
library(dplyr)
library(data.table)
library(purrr)
library(ggplot2)
library(corrplot)

train <- read.csv('../data/BankCamp_train.csv')
submission <- read.csv('../data/BankCamp_test.csv')

# check if there are any missing values in the training set and the submission set
# as both return FALSE, there are no missing values
any(is.na(train))
any(is.na(submission))

# check for the structure of the dataset
# comparing to training set with the submission set, we can tell the target variable is y
glimpse(train)
glimpse(submission)

# check for the factor levels of y
# as shown, it returns only yes and no, so it's a binary classification problem
unique(train$y)

features <- names(train)
features <- features[features != 'y']

# histogram for all the numerical features
train %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
# as shown from the hists, most of the numerical features are heavily skewed, and need to be normalized

# study the correlation between two numerical features
# first select the only numerical features
train_num <- dplyr::select_if(train, is.numeric)
# then calculate the correlation matrix and plot
res <- cor(train_num)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# as shown from the result, there are no strong correlations between most of the pairs
# except the one for pdays and previous = 0.5405558325

# to take a look of the factor features
train_factor <- dplyr::select_if(train, is.factor)

train %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_scatter()
