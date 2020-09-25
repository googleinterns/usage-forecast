# Copyright 2020 Google LLC
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# https://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
rm(list = ls())
######################################
##### Section 1: Import packages #####
######################################
library(forecast)
library(bsts)
library(glmnet)
library(plyr)
library(lme4)
library(strucchange)
library(changepoint)
library(mice)
source("functions_v3.R")

#####################################################
##### Section 2: Preprocessing for Second Split #####
#####################################################

### Impute data ###
# Remove future data.
change_date <- as.Date("2018-06-21", tz = "America/Los_Angeles")

# Impute range: 2017-08-01 to 2017-09-01.
# Read the original data to get the memory information.
data_agg_sum <- readRDS("data_agg_sum.rds")
data_agg_sum_trans_log <- readRDS("data_agg_sum_trans_log.rds")

### Data cleanning ###
# Remove observations that is unusual higher than others.
data_agg_sum_trans_log <- data_agg_sum_trans_log[data_agg_sum_trans_log$date <
                                                   as.Date("2018-07-21", tz = "America/Los_Angeles"),]
upper_clean_gcu <- quantile(data_agg_sum_trans_log$gcu_seconds, 0.999)
lower_clean_gcu <- quantile(data_agg_sum_trans_log$gcu_seconds, 0.001)
upper_clean_mem <- quantile(data_agg_sum_trans_log$memory_gib_seconds, 0.995)
lower_clean_mem <- quantile(data_agg_sum_trans_log$memory_gib_seconds, 0.005)

ind_clean_gcu <- which(data_agg_sum_trans_log$gcu_seconds > upper_clean_gcu | 
                         data_agg_sum_trans_log$gcu_seconds < lower_clean_gcu)
ind_clean_mem <- which(data_agg_sum_trans_log$memory_gib_seconds > upper_clean_mem | 
                         data_agg_sum_trans_log$memory_gib_seconds < lower_clean_mem)

missingdata <- as.data.frame(c(ind_clean_gcu, ind_clean_mem))
colnames(missingdata) <- "index"
missingdata$features <- rep(c("gcu_seconds", "memory_gib_seconds"), c(length(ind_clean_gcu), length(ind_clean_mem)))
missingdata$original_data <- 0
missingdata$original_data[1:length(ind_clean_gcu)] <- data_agg_sum_trans_log$gcu_seconds[ind_clean_gcu]
missingdata$original_data[(length(ind_clean_gcu)+1):nrow(missingdata)] <- data_agg_sum_trans_log$memory_gib_seconds[ind_clean_mem]
missingdata$date <- data_agg_sum_trans_log[missingdata$index, "date"]

# Set these observations to NA for imputation.
data_agg_sum_trans_log_prep <- GetTimeInfo(data_agg_sum_trans_log)
data_agg_sum_trans_log_prep$memory_gib_seconds[ind_clean_mem] <- NA
data_agg_sum_trans_log_prep$gcu_seconds[ind_clean_gcu] <- NA

# Select the features except cell, date, and gpu to impute.
data_agg_sum_trans_log_prep_mat <-as.matrix(cbind(data_agg_sum_trans_log_prep[, 5], data_agg_sum_trans_log_prep[, 6]))
set.seed(123)
imp_prep <- mice::mice(data_agg_sum_trans_log_prep[,1:6], meth="cart")
imp_prep2 <- mice::complete(imp_prep)
str(imp_prep2)
data_agg_sum_trans_log_prep[, 1:6] <- imp_prep2

### De-seasonality ###
# Transformed data de-seasonality
data_agg_sum_trans_deaseason <- de_seasonal(data_agg_sum_trans_log, 
                                       features = c("gcu_seconds", "memory_gib_seconds"), 
                                       season = 7,
                                       type = "diff")

### Split data ###
# Get two week time after June 21, 2018 as the comparison set
data_log_impute <- data_agg_sum_trans_deaseason
hold_date <- 7
end_date <- change_date + hold_date
comparison_set <- data_log_impute[data_log_impute$date < end_date & 
                                    data_log_impute$date >= change_date,]

# Get one month time before June 21, 2018 as the hold out set to use walk forward validation test model
data_log_impute <- data_agg_sum_trans_deaseason
hold_date <- 7
end_date <- change_date - hold_date
train_set_all <- data_log_impute[data_log_impute$date < end_date,]
holddout_set <- data_log_impute[data_log_impute$date >= end_date,]

###############################
##### Section 3: Modeling #####
###############################
### Check True Prediction for all data including data before 06212018 ### 
validation_set_all <- holddout_set[holddout_set$date < change_date,]
x_feature <- c("gcu_seconds", "memory_gib_seconds")
lag_num <- 21
tuning_set = train_set_all
initial_data <- data_agg_sum[data_agg_sum$date == (train_set_all[1,]$date-1), ]
var_pred <- ValidateModel(validate_type = "moving",
                          model = VarModel, 
                          tuning_set = train_set_all,
                          validation_set = validation_set_all, 
                          validation_interval = 7,
                          metric = "MSE", 
                          is.plot = TRUE, 
                          is.origin = T, 
                          transform.fun = Backtransform, 
                          initial_data = initial_data,
                          tuning_interval = 7, 
                          lambda = 10^seq(-4,0,0.01),
                          hold_day = 21, 
                          lag_num = lag_num, 
                          x_feature = x_feature,
                          is.together = F, 
                          weight_type = "weighted_uniform",
                          x_pred = T, 
                          current_add = F,
                          is.tuning = T,
                          season = 7,
                          missingdata = missingdata
)

var_pred$Metric$mse

### Prediction the contextual GCU and Memory ### 
# Get prediction
true_trainset_all <- rbind(train_set_all, 
                           holddout_set[holddout_set$date < change_date,])
var_pred_est <- ValidateModel(validate_type = "moving",
                              model = VarModel, 
                              tuning_set = true_trainset_all,
                              validation_set = comparison_set, 
                              validation_interval = 7,
                              metric = "MSE", 
                              is.plot = TRUE, 
                              is.origin = T, 
                              transform.fun = Backtransform, 
                              initial_data = initial_data,
                              tuning_interval = 7, 
                              lambda = 10^seq(-4,0,0.01), 
                              hold_day = 21, 
                              lag_num = lag_num, 
                              x_feature = x_feature,
                              is.together = F, 
                              weight_type = "weighted_uniform",
                              x_pred = T, 
                              current_add = F,
                              is.tuning = T,
                              season = 7,
                              missingdata = missingdata
)

var_pred_est$Metric$mse
str(var_pred_est$model)

### Analyse the prediction of contextual GCU and Memory ### 
# Get the change rate
prediction_res_total <- rbind(var_pred$prediction_org, var_pred_est$prediction_org)
actual_data <- data_agg_sum[change_date <=data_agg_sum$date &
                              data_agg_sum$date < change_date + 7, 
                            c("gcu_seconds", "memory_gib_seconds", "date")] 

pred_data <- var_pred_est$prediction_org
sum(pred_data$gcu_seconds - actual_data$gcu_seconds) / sum(pred_data$gcu_seconds) # 0.006606527
sum(pred_data$memory_gib_seconds - actual_data$memory_gib_seconds) / sum(pred_data$memory_gib_seconds) #-0.0007614263

# Zoom in plots
features <- c("gcu_seconds", "memory_gib_seconds")
actual_set <- data_agg_sum[2:nrow(data_agg_sum), ]
var_pred_res <- var_pred$prediction_org
var_pred_ind <- actual_set$date < var_pred_res$date[1]
var_pred_res <- rbind(actual_set[var_pred_ind, c(features, "date")], 
                      var_pred_res)

var_pred_est_res <- var_pred_est$prediction_org
var_pred_est_ind <- actual_set$date < var_pred_est_res$date[1]
var_pred_est_res <- rbind(actual_set[var_pred_est_ind, c(features, "date")], 
                          var_pred_est_res)

GetZoomIn(dataset = var_pred_res, 
          actual_set = actual_set, 
          interval = c((nrow(var_pred_res) - 7 - 29): nrow(var_pred_res)),
          breakline = as.Date("2018-06-13"),
          features_name = features,
          features_lab = c("GCU-Seconds", "RAM (GiB) - Seconds")) 

GetZoomIn(dataset = var_pred_est_res, 
          actual_set = actual_set, 
          interval = c((nrow(var_pred_est_res) - 7 - 29): nrow(var_pred_est_res)),
          breakline = as.Date("2018-06-20"),
          features_name = features,
          features_lab = c("GCU-Seconds", "RAM (GiB) - Seconds")) 

# Get cofidence intervals
model_fit = var_pred_est$model[[1]]
prediction_org <- var_pred_est$prediction_org
dataset = true_trainset_all[(season + 1):nrow(true_trainset_all), c(features, "date")]
data_trans <- data_agg_sum_trans_log
data_org <- data_agg_sum

ci_res <- GetConf(model_fit, 
                  dataset, 
                  data_trans,
                  data_org,
                  type = "var", 
                  lag_num = lag_num, 
                  interval = 7, 
                  features = x_feature, 
                  transform.fun = Backtransform,
                  CI = c(0.025, 0.975),
                  prediction_org,
                  x_pred = T,
                  is.together = F,
                  current_add = F) 

par(mfrow = c(1,1))
plot(prediction_org$date, prediction_org$gcu_seconds, type = 'l', 
     ylim = c(min(c(ci_res$CI_day[[1]][,1], actual_data$gcu_seconds)), 
              max(c(ci_res$CI_day[[1]][,2], actual_data$gcu_seconds))),
     main = "Daily Confidence Interval", xlab = "Time", ylab = "GCU-Seconds", 
     col = 2)
lines(prediction_org$date, ci_res$CI_day[[1]][,1], col = 3, lty = 2)
lines(prediction_org$date, ci_res$CI_day[[1]][,2], col = 3, lty = 2)
lines(as.Date(actual_data$date), actual_data$gcu_seconds, col = 1)
legend("bottomright", 
       legend = c("Actual Values", "Predicted Values", "CI"), 
       col = 1:3, lty = c(1, 1, 2), cex = 0.5)
sum(actual_data$gcu_seconds)
sum(prediction_org$gcu_seconds)
ci_res$CI_sum[[1]]

plot(prediction_org$date, prediction_org$memory_gib_seconds, type = 'l', 
     ylim = c(min(ci_res$CI_day[[2]][,1]), max(ci_res$CI_day[[2]][,2])),
     main = "Daily Confidence Interval", xlab = "Time", ylab = "RAM (GiB) - Seconds",
     col = 2)
lines(prediction_org$date, ci_res$CI_day[[2]][,1], col = 3, lty = 2)
lines(prediction_org$date, ci_res$CI_day[[2]][,2], col = 3, lty = 2)
lines(as.Date(actual_data$date), actual_data$memory_gib_seconds, col = 1)
legend("bottomright", 
       legend = c("Actual Values", "Predicted Values", "CI"), 
       col = 1:3, lty = c(1, 1, 2), cex = 0.5)
sum(actual_data$memory_gib_seconds)
sum(prediction_org$memory_gib_seconds)
ci_res$CI_sum[[2]]