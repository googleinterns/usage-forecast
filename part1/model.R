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
source("functions.R")
# Import packages ---------------------------
library(forecast)
library(bsts)
library(glmnet)
library(plyr)

# Read transformed daily data---------------------------
data_agg_1day_trans_log <- readRDS("data_agg_1day_trans_log.rds")

# Read original daily data
info_table_1day_clean_agg <- readRDS("info_table_1day_clean_agg.rds")

# Remove future data
end_date <- as.POSIXlt("2018-03-28", tz = "GMT")
data_log <- data_agg_1day_trans_log[data_agg_1day_trans_log$date < end_date,]

# Impute data---------------------------
# Impute range: 2017-08-01 to 2017-09-01
# Remove the wrong difference, since that value is the difference between 
# 2017-09-01 and 2017-07-31
impute_rm_date <- "2017-09-01"
data_log_impute_prep <- data_log[format(data_log$date,"%Y-%m-%d") != impute_rm_date,]
impute_rm_date <- "2017-09-02"
data_log_impute_prep <- data_log_impute_prep[format(data_log_impute_prep$date,"%Y-%m-%d") != impute_rm_date,]

impute_rm_org <- info_table_1day_clean_agg[format(info_table_1day_clean_agg$date,"%Y-%m-%d") >= impute_rm_date &
                                             format(info_table_1day_clean_agg$date,"%Y-%m-%d") < end_date, ]
data_log_impute_prep$day <- format(data_log_impute_prep$date,"%d")
data_log_impute_prep$month <- format(data_log_impute_prep$date,"%m")
data_log_impute_prep$year <- format(data_log_impute_prep$date,"%Y")
data_log_impute_prep$weekday <- format(data_log_impute_prep$date, "%a")
data_log_impute_prep <- data_log_impute_prep[data_log_impute_prep$month %in% 
                                               c("07", "08", "09"),]
features_select <- c("duration", "instances", "gcu_seconds", 
                     "memory_gib_seconds", "disk_gib_seconds")

data_log_impute_prep_summary <- lapply(features_select, function(x) GetAvgBygroup(dataset = subset(data_log_impute_prep, select = -date), 
                                                                                  feature = x, group = "day"))

data_log_impute <-data_log_impute_prep_summary[[1]]
data_log_impute$date <- as.POSIXlt(paste("2017-08-", data_log_impute$day, sep = ""),
                                   tz = "GMT")
year_avg <- apply(data_log_impute_prep[data_log_impute_prep$year == "2017", features_select], 
                  2, mean)
weekday_avg <- lapply(features_select, function(x) GetAvgBygroup(dataset = subset(data_log_impute_prep, select = -date), 
                                                                 feature = x, group = "weekday"))

for (i in 1:length(data_log_impute_prep_summary)) {
  data_log_impute[,features_select[i]] <- data_log_impute_prep_summary[[i]]$mean +
    year_avg[[i]] 
  
  weekday_names <- weekday_avg[[1]]$weekday
  for (j in 1:length(weekday_names)){
    data_log_impute[format(data_log_impute$date, "%a") == weekday_names[j],features_select[i]] <- 
      data_log_impute[format(data_log_impute$date, "%a") == weekday_names[j],features_select[i]] + 
      weekday_avg[[i]][j, "mean"]
  }
}

data_log_impute <- data_log_impute[, 4:ncol(data_log_impute)]
head(data_log_impute)
data_log_impute <- rbind(data_log_impute, data_log)
data_log_impute <- data_log_impute[order(data_log_impute$date),]
data_log_impute2 <- data_log_impute
data_org_impute <- Backtransform(dataset = data_log_impute, initial_data = info_table_1day_clean_agg[1,], features = features_select,
                          tranform_type = "log_diff")
data_org_impute$date <- data_log_impute$date
data_org_impute <- rbind(info_table_1day_clean_agg[1,], data_org_impute)
data_org_impute[format(data_org_impute$date, "%Y-%m-%d") >= impute_rm_date, ] <- impute_rm_org

data_log_impute <- log(data_org_impute[2:nrow(data_org_impute), features_select]) - 
  log(data_org_impute[1:(nrow(data_org_impute) - 1), features_select])
data_log_impute$date <- data_org_impute$date[2:nrow(data_org_impute)]
# Check the backtransformation
plot(data_org_impute$date, data_org_impute$gcu_seconds, type = "l", col = 2)
tmp2 <- info_table_1day_clean_agg[info_table_1day_clean_agg$date <= end_date,]
lines(tmp2$date, tmp2[, "gcu_seconds"])

GetLineplot(dataset = data_log_impute, filepath = "plots/impute_") 

# Split data---------------------------
# Get three month time before March 28, 2018 as the hold out set to use walk forward validation test model
#
hold_date <- 90
end_date <- as.Date(as.POSIXlt("2018-03-28", tz = "GMT")) - hold_date
train_set_all <- data_log_impute[as.Date(data_log_impute$date) < end_date,]
holddout_set <- data_log_impute[as.Date(data_log_impute$date) >= end_date,]

# Get two month time before previous end_date as the validation set to use walk forward validation to select models
#
hold_date <- 60
end_date <- end_date - hold_date
tuning_set <- train_set_all[as.Date(train_set_all$date) < end_date,]
validation_set <- train_set_all[as.Date(train_set_all$date) >= end_date,]

# Get one month time before previous end_date as the train set to train models and parameter tuning
#
hold_date <- 30
end_date <- end_date - hold_date
train_set_tune <- tuning_set[as.Date(tuning_set$date) < end_date,]
test_set_tune <- tuning_set[as.Date(tuning_set$date) >= end_date,]
  
# Modeling---------------------------
# ARIMA
# Check acf and pacf plots for lag selection
features_select_disk_rm <- features_select[1:4]
for (i in features_select_disk_rm) {
  png(paste("plots/acf_tune_", i, ".png", sep = ""))
  par(mfrow=c(2,1))
  acf(tuning_set[,i])
  pacf(tuning_set[,i])
  dev.off()
}

# acf plot after remove year 2016
for (i in features_select_disk_rm) {
  png(paste("plots/acf_tune_16rm_", i, ".png", sep = ""))
  par(mfrow=c(2,1))
  acf(tuning_set[366:nrow(tuning_set),i])
  pacf(tuning_set[366:nrow(tuning_set),i])
  dev.off()
}



# # Check VarModel
# work
y_feature = c("gcu_seconds", "memory_gib_seconds")
x_feature = c("duration", "instances", "gcu_seconds", "memory_gib_seconds")
dataset = tuning_set
forcast_time = 30
tuning_interval = 10
x_pred = T
current_add = F
is.together = F
VarModel(dataset = tuning_set, tuning_interval = 10, lambda = seq(0.001,0.05, length.out = 20),
                     hold_day = 30, lag_num = 15, x_feature = x_feature, y_feature = y_feature,
                     is.together = F, weight_type = "equal", forcast_time = forcast_time, predict_set = validation_set)


# Validate---------------------------
# Check ValidateModel---------------------------
initial_data <- info_table_1day_clean_agg[info_table_1day_clean_agg$date < tuning_set[1,]$date, ]
initial_data <- initial_data[nrow(initial_data),]
# arima_res <- ValidateModel(model = ArimaModel, 
#                            tuning_set = tuning_set, 
#                            validation_set = validation_set, 
#                            validation_interval = 30,
#                            metric = "MSE", 
#                            is.plot = TRUE, 
#                            is.origin = T, 
#                            transform.fun = Backtransform, 
#                            initial_data = initial_data,
#                            p = 11,
#                            q = 0,
#                            is.tuning = F
#                            )





# Compare ValidateModel---------------------------
# Select the best VAR model
res_notogether <- GetTunValTable(tuning_set, is.together = F, lag_num = 18) 
res_notogether_16rm <- GetTunValTable(tuning_set[366:nrow(tuning_set),], is.together = F, lag_num = 12) 
res_together <- GetTunValTable(tuning_set, is.together = T, lag_num = 18) 
res_together_16rm <- GetTunValTable(tuning_set[366:nrow(tuning_set),], is.together = T, lag_num = 12) 
res_tunval_table <- rbind(res_notogether, res_notogether_16rm, res_together, res_together_16rm)
colnames(res_tunval_table) <- c(3, 7, 14, 30)
rownames(res_tunval_table) <- rep(c("MSE_gcu", "MSE_memory"), 8)
saveRDS(res_tunval_table, "tables/res_tunval_table.rds")
res_tunval_table

res_together_16rm_weighted <- GetTunValTable(tuning_set[366:nrow(tuning_set),], 
                                             is.together = T, lag_num = 12, weight_type = "else") 


# Compare different models
model_arima <-  GetModelValTable(ArimaModel, 
                                 is.tuning = F,
                                 p = 18,
                                 q = 0
)
lag_num = 25
model_var <-  GetModelValTable(VarModel,
                               is.tuning = T,
                               tuning_interval = 7, 
                               # lambda = seq(0.0025,0.1,0.0025),
                               # lambda = seq(0.001,0.5,0.001),
                               lambda = seq(0.02,0.04,0.001),
                               hold_day = 30, 
                               lag_num = lag_num, 
                               x_feature = x_feature, 
                               is.together = T, 
                               weight_type = "equal",
                               x_pred = T, 
                               current_add = F)

model_bays <-  GetModelValTable(BaysianModel, 
                                is.tuning = F)



model_com_res <- rbind(model_arima, model_var, model_bays)
colnames(model_com_res) <- c(3, 7, 14, 30)
rownames(model_com_res) <- rep(c("MSE_gcu", "MSE_memory"),3)
saveRDS(model_com_res, "tables/model_com_res.rds")
