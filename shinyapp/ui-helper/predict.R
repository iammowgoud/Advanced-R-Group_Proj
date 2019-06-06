
load_reqs <- function(reqs) {
  for(pkg in reqs) {
    if (!(pkg %in% installed.packages())) { install.packages(pkg) }
    suppressPackageStartupMessages(library(pkg, character.only = T))
  }
}

# ADD LIBRARIES HERE
load_reqs(c("dplyr", "data.table", "ggplot2", "corrplot", "gridExtra", "DT",
            "grid", "doParallel", "caret", "MLmetrics", "DMwR", "car", "stats",
            "rstudioapi", "ranger", "xgboost", "deepboost", "highcharter","purrr", "tidyr","cowplot"))


to_log <- function(df, l_varlist){
  for(l_var in l_varlist){
    df[,l_var] <- log(df[,l_var])
  }
  return(df)
}

# Select the numeric variables that are skewed
log_varlist <- list('duration', 'campaign', 'previous')



predict_data <- function(submission) {
  

  ## train data
  library(rstudioapi)
  
  df_train_preproc <- read.csv("train_preproc.csv")

  temp <- df_train_preproc
  
  df_train_preproc$X = NULL
  
  df_train_preproc$label <- as.factor(df_train_preproc$label)
  df_train_preproc$duration <- as.numeric(df_train_preproc$duration)
  
  df_train_preproc$campaign <- as.numeric(df_train_preproc$campaign)
  df_train_preproc$previous <- as.numeric(df_train_preproc$previous)

  ## Final Prediction


  submission$day <- as.factor(submission$day)
  submission$age_bin <- cut(submission$age, breaks = c(-Inf, 20, 40, 60, Inf),
                            labels = c('<20', "20-40", "40-60", ">60"))
  submission$age <- NULL
  submission$job_binned <- car::recode(submission$job, "c('retired', 'student') = 'rs';
                                       c('blue-collar', 'entrepreneur')= 'be';
                                       c('housemaid', 'services')= 'hs';
                                       c('admin.', 'unknown')= 'au';
                                       c('management', 'self-employed','technician', 'unemployed') = 'others'")
  submission$job_binned <- factor(submission$job_binned)
  submission$job <- NULL
  submission$balance_bin <- cut(submission$balance,
                                breaks = c(-Inf, 0, 500, 1000, 5000, Inf),
                                labels = c("<=0", "1-500", "501-1000", "1001-5000", ">5000"))

  submission$balance <- NULL
  submission$day_binned <- car::recode(submission$day, "c(1,30) = 'special';
                                       c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                                       22,23,24,25,26,27,28,29,31) = 'others'")
  submission$day_binned <- factor(submission$day_binned)
  submission$pdays_binned <- cut(submission$pdays, breaks = c(-Inf, 0, 99, 299, 499, Inf ),
                                 labels = c("notcontacted", "0-100", "101-300", "301-500", ">500"))
  submission$pdays <- NULL

  submission$day <- NULL
  submission_log <- submission
  # Add constant in order to avoid non-positive values in log (monotonic transformation)
  submission_log$duration <- submission_log$duration + 1
  #submission_log$pdays <- submission_log$pdays + 2
  submission_log$previous <- submission_log$previous + 1

  submission_log <- to_log(submission_log, log_varlist)

  submission <- submission_log



  ### MODEL
  recallSummary <- function (data,
                             lev = NULL,
                             model = NULL) {
    c(Accuracy = MLmetrics::Accuracy(data$pred, data$obs),
      recall = MLmetrics::Recall(data$obs, data$pred, positive = 1))
  }

  ctrl <- trainControl(method = "cv",
                       number = 5,
                       savePredictions=TRUE,
                       summaryFunction = recallSummary,
                       sampling = "down")

  ranger_finalGrid <- expand.grid(mtry=round(1/2 * (length(df_train_preproc))),
                                  splitrule= 'gini',
                                  min.node.size=200)
  set.seed(7)

  ranger_final <- train(label~.,
                        data = df_train_preproc,
                        method = "ranger", num.trees=500,
                        tuneGrid = ranger_finalGrid,
                        metric = "recall",
                        importance = "permutation",
                        trControl = ctrl)

  final_pred <- predict(ranger_final, data = submission)
  
  
  temp$LABEL <- as.factor(ifelse(final_pred == 1, 'yes', 'no'))

  return(temp)
}

# 
# input_data =  readr::read_csv("/Users/tommy/git/IE/TERM-3/Advanced R/repo/data/BankCamp_test.csv")
# 
# prediction = predict_data(input_data)
# 

