train <- read.csv("../data/BankCamp_train.csv")

eda_data <- as.data.table(train)
eda_data$label <- as.factor(ifelse(eda_data$y == 'yes', 1, 0))
eda_data$label_edit <- as.factor(ifelse(eda_data$y == 'yes', "Yes", "No"))
eda_data$y <- NULL

numeric_vars <- unlist(lapply(eda_data, is.numeric)) 
cat_vars <- unlist(lapply(eda_data, is.factor)) 

## age

plot_age <- hchart(density(eda_data[label_edit == "Yes"]$age), name = "Yes", type = "line") %>%
  hc_add_series(density(eda_data[label_edit == "No"]$age), type = "line", name = "No") %>%
  hc_title(text = "Density plot for 'age'")

## jobs
eda_data_job <- eda_data %>% 
  group_by(job) %>% 
  summarise(n = n(), unique = length(unique(job))) %>% 
  arrange(-n, -unique) %>% 
  glimpse()
plot_job <- hchart(eda_data_job, "treemap", hcaes(x = job, value = n, color = n)) %>%
  hc_title(text = "Clients Jobs Tree Map")
plot_job
### function name and counts
names_counts <- function(eda_data_agg){
  nc <- list()
  nc$names <- as.character(unlist(eda_data_agg[,1]))
  nc$counts <- as.numeric(unlist(eda_data_agg[,2]))
  return(nc)
}

###### waffle
## marital
eda_data_marital <- eda_data %>% 
  group_by(marital) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  glimpse()

plot_marital <- hciconarray(names_counts(eda_data_marital)$names, round(names_counts(eda_data_marital)$counts / 500), size = 5) %>%
  hc_title(text = "Clients Marital Status(1 unit = 500 Clients)")

## housing
eda_data_housing <- eda_data %>% 
  group_by(housing) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  glimpse()


plot_housing <- hciconarray(names_counts(eda_data_housing)$names, round(names_counts(eda_data_housing)$counts / 500), size = 5) %>%
  hc_title(text = "Clients Housing Status(1 unit = 500 Clients)")


## default
eda_data_default <- eda_data %>% 
  group_by(default) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  glimpse()

plot_default <- hciconarray(names_counts(eda_data_default)$names, round(names_counts(eda_data_default)$counts / 500), size = 5) %>%
  hc_title(text = "Clients Default Status(1 unit = 500 Clients)")


## contact
eda_data_contact <- eda_data %>% 
  group_by(contact) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  glimpse()

plot_contact<- hciconarray(names_counts(eda_data_contact)$names, round(names_counts(eda_data_contact)$counts / 500), size = 5) %>%
  hc_title(text = "Contact Channel Distribution(1 unit = 500 Clients)")


## education
eda_data_education <- eda_data %>% 
  group_by(education) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  glimpse()

plot_education <- hciconarray(names_counts(eda_data_education)$names, round(names_counts(eda_data_education)$counts / 500), size = 5) %>%
  hc_title(text = "Education Level Distribution(1 unit = 500 Clients)")


## balance

plot_balance <- hchart(density(eda_data[label_edit == "Yes"]$balance), name = "Yes", type = "line") %>%
  hc_add_series(density(eda_data[label_edit == "No"]$balance), type = "line", name = "No") %>%
  hc_title(text = "Density plot for 'balance'")

## loan
eda_data_loan <- eda_data %>% 
  group_by(loan) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  glimpse()

plot_loan <- hciconarray(names_counts(eda_data_loan)$names, round(names_counts(eda_data_loan)$counts / 500), size = 5) %>%
  hc_title(text = "Loan Status Distribution(1 unit = 500 Clients)")

## day

plot_day <- hchart(density(eda_data[label_edit == "Yes"]$day), name = "Yes", type = "line") %>%
  hc_add_series(density(eda_data[label_edit == "No"]$day), type = "line", name = "No") %>%
  hc_title(text = "Density plot for 'day'")

## month

eda_data_month <- eda_data %>% 
  group_by(month) %>% 
  summarise(n = n(), unique = length(unique(month))) %>% 
  arrange(-n, -unique) %>% 
  glimpse()
plot_month <- hchart(eda_data_month, "treemap", hcaes(x = month, value = n, color = n)) %>%
  hc_title(text = "Month Tree Map")

#plot_month <- hchart(density(eda_data[label_edit == "Yes"]$month), name = "Yes", type = "line") %>%
# hc_add_series(density(eda_data[label_edit == "No"]$month), type = "line", name = "No")

## duration

plot_duration <- hchart(density(eda_data[label_edit == "Yes"]$duration), name = "Yes", type = "line") %>%
  hc_add_series(density(eda_data[label_edit == "No"]$duration), type = "line", name = "No") %>%
  hc_title(text = "Density plot for 'duration'")

## campaign

plot_campaign <- hchart(density(eda_data[label_edit == "Yes"]$campaign), name = "Yes", type = "line") %>%
  hc_add_series(density(eda_data[label_edit == "No"]$campaign), type = "line", name = "No") %>%
  hc_title(text = "Density plot for 'campaign'")

## pdays

plot_pdays <- hchart(density(eda_data[label_edit == "Yes"]$pdays), name = "Yes", type = "line") %>%
  hc_add_series(density(eda_data[label_edit == "No"]$pdays), type = "line", name = "No") %>%
  hc_title(text = "Density plot for 'pdays'")

## previous

plot_previous <- hchart(density(eda_data[label_edit == "Yes"]$previous), name = "Yes", type = "line") %>%
  hc_add_series(density(eda_data[label_edit == "No"]$previous), type = "line", name = "No") %>%
  hc_title(text = "Density plot for 'previous'")

## poutcome
eda_data_poutcome <- eda_data %>% 
  group_by(poutcome) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  glimpse()

plot_poutcome <- hciconarray(names_counts(eda_data_poutcome)$names, round(names_counts(eda_data_poutcome)$counts / 500), size = 5) %>%
  hc_title(text = "Poutcome Distribution(1 unit = 500 Clients)")

# corr matrix
# fntltp <- JS("function(){
#              return this.series.xAxis.categories[this.point.x] + ' ' +  this.series.yAxis.categories[this.point.y] + ':<br>' +
#              Highcharts.numberFormat(this.point.value, 2);
#              }")
# 
# corr_matrix < - hchart(res_l, "heatmap", hcaes(x = Var1, y = Var2, value = value)) %>% 
#   hc_colorAxis(minColor="#8EDBFF", maxColor="#8296FF") %>% 
#   hc_yAxis(reversed = TRUE, offset = 0, tickLength = 0,
#            gridLineWidth = 0, minorGridLineWidth = 0,
#            labels = list(style = list(fontSize = "8px"))) %>% 
#   hc_tooltip(formatter = fntltp) %>% 
#   hc_title(text = "Correlation Matrix") %>% 
#   hc_legend(layout = "vertical", verticalAlign = "top",
#             align = "right", valueDecimals = 0) %>%
#   hc_add_theme(hc_theme_monokai())

# LOAD PREVIOUSLY FIT MODELS
log_up_r <- readRDS("log_up.rds")
ranger_r <- readRDS("ranger.rds")

#######################MODEL FITTING CODE######################

# # encode target variable as 1 and 0 and convert it to factor
# train$label <- as.factor(ifelse(train$y == 'yes', 1, 0))
# train$y <- NULL
# 
# # split train data further into train and test (hold out)
# test_proportion <- 0.2
# set.seed(7)
# train_index<-sample(nrow(train), floor(nrow(train)*(1-test_proportion)), replace = FALSE)
# 
# df_train <- train[train_index,]
# df_test <- train[-train_index,]
# 
# ### Baseline with Resampling
# # Define function
# recallSummary <- function (data,
#                            lev = NULL,
#                            model = NULL) {
#   c(Accuracy = MLmetrics::Accuracy(data$pred, data$obs),
#     recall = MLmetrics::Recall(data$obs, data$pred, positive = 1))
# }
# 
# 
# #### Oversampling but no feature preprocessing
# # define basic train control object 
# ctrl <- trainControl(method = "cv",
#                      number = 5,
#                      savePredictions=TRUE,
#                      summaryFunction = recallSummary,
#                      sampling = "up")
# 
# set.seed(7)
# log_up <- train(label~.,
#                 data = df_train,
#                 method = 'glm',
#                 family = "binomial",
#                 trControl = ctrl)
# 
# # Plot confusion matrix
cm <- confusionMatrix(log_up_r)
data <- as.data.frame(cm$table)
cmplotout <- ggplot(data = data , mapping = aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = paste0(round(Freq,0), "%")), vjust = 1) +
  scale_fill_gradient(low = "deepskyblue4", high = "yellowgreen") +
  theme_bw() + theme(legend.position = "none")
 
# ## Random forest
# 
# ### Feature Preprocessing
# train_preproc <- train
# # day as a categorical variable  
# train_preproc$day <- as.factor(train$day)
# # bin age variable
# train_preproc$age_bin <- cut(train_preproc$age, breaks = c(-Inf, 20, 40, 60, Inf),
#                              labels = c('<20', "20-40", "40-60", ">60"))
# train_preproc$age <- NULL
# 
# # reduce job categories
# train_preproc$job_binned <- car::recode(train_preproc$job, "c('retired', 'student') = 'rs';
#                                         c('blue-collar', 'entrepreneur')= 'be';
#                                         c('housemaid', 'services')= 'hs'; 
#                                         c('admin.', 'unknown')= 'au';
#                                         c('management', 'self-employed','technician', 'unemployed') = 'others'")
# train_preproc$job_binned <- factor(train_preproc$job_binned)
# 
# train_preproc$job <- NULL
# # balance to categorical
# train_preproc$balance_bin <- cut(train_preproc$balance, 
#                                  breaks = c(-Inf, 0, 500, 1000, 5000, Inf),
#                                  labels = c("<=0", "1-500", "501-1000", "1001-5000", ">5000"))
# 
# 
# train_preproc$balance <- NULL
# # modify days
# train_preproc$day_binned <- car::recode(train_preproc$day, "c(1,30) = 'special';
#                         c(2,3,4,5,6,7,8,9,10, 11,12,13,14,15,16,17,18,19,20,21,
#                                         22,23,24,25,26,27,28,29,31) = 'others'")
# train_preproc$day_binned <- factor(train_preproc$day_binned)
# 
# train_preproc$day <- NULL
# 
# # Logarithmise skewed variables
# to_log <- function(df, l_varlist){
#   for(l_var in l_varlist){
#     df[,l_var] <- log(df[,l_var])
#   }
#   return(df)
# }
# 
# # Select the numeric variables that are skewed
# log_varlist <- list('duration', 'campaign', 'pdays', 'previous')
# 
# train_log <- train_preproc
# 
# # Add constant in order to avoid non-positive values in log (monotonic transformation)
# train_log$duration <- train_log$duration + 1
# train_log$pdays <- train_log$pdays + 2
# train_log$previous <- train_log$previous + 1
# 
# train_log <- to_log(train_log, log_varlist)
# 
# df_train_preproc <- train_log[train_index,]
# df_test_preproc <- train_log[-train_index,]
# 
# # fit model
# 
# ctrlrf <- trainControl(method = "cv",
#                      number = 5,
#                      savePredictions=TRUE,
#                      summaryFunction = recallSummary,
#                      sampling = "up")
# 
# set.seed(7)
# tuneGrid_ranger <- data.table(expand.grid(mtry=c(round(sqrt(length(df_train_preproc)) + 1)
# ), #standard choices for mtry
# splitrule= 'gini',
# min.node.size=c(500, 700, 1000)))
# 
# ranger <- train(label~.,
#                 data = df_train_preproc,
#                 method = "ranger", num.trees=100,
#                 tuneGrid = tuneGrid_ranger,
#                 metric = "recall",
#                 trControl = ctrl, importance = 'permutation')
# 
# # plot confusion matrix
# # Plot confusion matrix
cmrf <- confusionMatrix(ranger_r)
datarf <- as.data.frame(cmrf$table)
cmrfplotout <- ggplot(data = datarf , mapping = aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = paste0(round(Freq,0), "%")), vjust = 1) +
  scale_fill_gradient(low = "deepskyblue4", high = "yellowgreen") +
  theme_bw() + theme(legend.position = "none")
# 
# # test accuracy
# mypred_ranger <- predict(ranger, newdata = df_test_preproc)
# MLmetrics::Accuracy(mypred_ranger, df_test_preproc$label)
# MLmetrics::Recall(df_test_preproc$label, mypred_ranger, positive = 1)
