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
# Exploratory analysis functions ---------------------------
# Summary: Create minutes, hour, day, week, month, year variables for summarization
# @dataset: dataframe with features, must include the feature `data`, which contains time information
# @time_window represents how many minutes in the time window
# @feature specified column in the dataset
# Return: dataframe with minutes, hour, day, week, month, year variables
GetSummaryTable <- function(dataset = data_agg_total, 
                            feature = "gcu_seconds",
                            time_window = 15){
  n <- dim(dataset)[1]
  time_start <- dataset$date[1]
  time_end <- dataset$date[n] + time_window*60
  time_stamp <- seq(time_start, time_end, by = time_window*60)
  dataset_tmp <- as.data.frame(dataset %>% 
                                 dplyr::group_by(date = cut(dataset$date, breaks = time_stamp)) %>% 
                                 dplyr::select(date, all_of(feature))
  )
  
  feature_sum <- plyr::ddply(dataset_tmp, 
                             .(date), 
                             summarize,
                             mean = mean(get(feature)), 
                             median = median(get(feature)),
                             sum = sum(get(feature)),
                             lower = quantile(get(feature), probs = 0.25),
                             upper = quantile(get(feature), probs = 0.95))
  time_info <- data.frame(cbind(format(time_stamp, "%b"),
                                format(time_stamp, "%a"),
                                format(time_stamp, "%d"),
                                format(time_stamp, "%y"),
                                format(time_stamp, "%H"),
                                format(time_stamp, "%H:%M"),
                                format(time_stamp, "%Y-%m-%d %H:%M:%S")
  ))
  colnames(time_info) <- c("Month", "Week", "Day", "Year", "Hour", "Minu", "date")
  
  time_info_merge = merge(time_info, feature_sum, by = "date")
  
  return(time_info_merge)
}

# Summary: Get the data summary for hour, day, week, month, year variables
# @info_table: dataframe with hour, day, week, month, year variables
# Return: list of timely summary statistics 
GetSummary <- function(info_table = info_table_gcu_second){
  month_summary = plyr::ddply(info_table, 
                              .(factor(Month)), 
                              summarize, 
                              mean = mean(sum),
                              median = median(sum),
                              sum_sum = sum(sum),
                              sd = sd(sum),
                              lower = quantile(sum, probs = 0.25),
                              upper = quantile(sum, probs = 0.95)
  )
  week_summary = plyr::ddply(info_table, 
                             .(Week), 
                             summarize, 
                             mean = mean(sum),
                             median = median(sum),
                             sum_sum = sum(sum),
                             sd = sd(sum),
                             lower = quantile(sum, probs = 0.25),
                             upper = quantile(sum, probs = 0.95)
  )
  day_summary = plyr::ddply(info_table, 
                            .(Day), 
                            summarize, 
                            mean = mean(sum),
                            median = median(sum),
                            sum_sum = sum(sum),
                            sd = sd(sum),
                            lower = quantile(sum, probs = 0.25),
                            upper = quantile(sum, probs = 0.95)
  )
  
  year_summary = plyr::ddply(info_table, 
                             .(Year), 
                             summarize, 
                             mean = mean(sum),
                             median = median(sum),
                             sum_sum = sum(sum),
                             sd = sd(sum),
                             lower = quantile(sum, probs = 0.25),
                             upper = quantile(sum, probs = 0.95)
  )
  
  hour_summary = plyr::ddply(info_table, 
                             .(Hour), 
                             summarize, 
                             mean = mean(sum),
                             median = median(sum),
                             sum_sum = sum(sum),
                             sd = sd(sum),
                             lower = quantile(sum, probs = 0.25),
                             upper = quantile(sum, probs = 0.95)
  )
  return(list("month_summary" = month_summary,
              "week_summary" = week_summary,
              "day_summary" = day_summary,
              "year_summary" = year_summary,
              "hour_summary" = hour_summary
  ))
}

# Sumamry: General line graph for each day
# @dataset: dataframe of time series, must include the feature date
# @filepath: filepath and name for the plots
# Return: Saved line graph for gcu, duration, instance, and memory
GetLineplot = function(dataset, filepath) {
  gg_gcu = ggplot(dataset, 
                  aes(x = as.Date(date), y = gcu_seconds)) + 
    geom_line() + 
    xlab("")
  
  
  gg_duration = ggplot(dataset, 
                       aes(x = as.Date(date), y = duration)) + 
    geom_line() + 
    xlab("")
  
  gg_instances = ggplot(dataset, 
                        aes(x = as.Date(date), y = instances)) + 
    geom_line() + 
    xlab("")
  
  gg_memory = ggplot(dataset, 
                     aes(x = as.Date(date), y = memory_gib_seconds)) + 
    geom_line() + 
    xlab("")
  
  ggsave(filename = paste(filepath,"gcu_duration_linegraph.png", sep = ""), 
         plot = grid.arrange(gg_gcu, gg_duration),
         width = 10, height = 8, dpi = 100)
  ggsave(filename = paste(filepath,"instances_memory_linegraph.png", sep = ""),
         plot = grid.arrange(gg_instances, gg_memory),
         width = 10, height = 8, dpi = 100)
}

# Sumamry: Box plot by month, weekdays, year, hour
# @dataset: a list of summary data of features
# @feature: the feature specified for boxplot
# @summary_stat: specify the summary statistic for boxplot
# @filepath: filepath and name for the plots
# Return: Saved box plots for specified feature across hour, day, week, month, and year
GetBoxplot = function(dataset, feature, summary_stat, filepath){
  dataset$Month = factor(dataset$Month, levels = month.abb)
  dataset$Week = factor(dataset$Week, 
                        levels = c("Mon", "Tue", "Wed",
                                   "Thu", "Fri", "Sat",
                                   "Sun"))
  
  box_gpu_month = ggplot(dataset, 
                         aes(x = Month, y = get(summary_stat))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature, "by Month"), 
         x = "Month",
         y = paste(feature, summary_stat))
  
  box_gpu_week = ggplot(dataset, 
                        aes(x = Week, y  = get(summary_stat))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature, "by Weekday"), 
         x = "Weekday",
         y = paste(feature, summary_stat))  
  
  box_gpu_year = ggplot(dataset, 
                        aes(x = Year, y = get(summary_stat))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature, "by Year"), 
         x = "Year",
         y = paste(feature, summary_stat)) 
  
  box_gpu_hour = ggplot(dataset, 
                        aes(x = Hour, y = get(summary_stat))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature, "by Hour"), 
         x = "Hour",
         y = paste(feature, summary_stat)) 
  
  ggsave(filename=paste(filepath, feature, "_", summary_stat, "_Boxplot.png", sep = ""), 
         plot=grid.arrange(box_gpu_hour, 
                           box_gpu_week,
                           box_gpu_month,
                           box_gpu_year),
         width = 10, height = 8, dpi = 100)
  
}

# Sumamry: Bar plot by month, weekdays, year, hour
# @dataset: a list of summary data of features
# @feature: the feature specified for boxplot
# @summary_stat: specify the summary statistic for boxplot
# @filepath: filepath and name for the plots
# Return: Saved bar plots for specified feature across hour, day, week, month, and year
GetBarplot = function(dataset, feature, summary_stat, filepath){
  dataset$Month = factor(dataset$Month, levels = month.abb)
  dataset$Week = factor(dataset$Week, 
                        levels = c("Mon", "Tue", "Wed",
                                   "Thu", "Fri", "Sat",
                                   "Sun"))
  
  bar_gpu_month = ggplot(dataset, 
                         aes(x = Month, y = get(summary_stat))) + 
    stat_summary(fun = sum, geom = "bar") + 
    labs(tittle = "Barplot", 
         subtitle = paste(feature, "by Month"), 
         x = "Month",
         y = paste(feature, summary_stat))
  
  bar_gpu_week = ggplot(dataset, 
                        aes(x = Week, y = get(summary_stat))) + 
    stat_summary(fun = sum, geom = "bar") + 
    labs(tittle = "Barplot", 
         subtitle = paste(feature, "by Weekday"), 
         x = "Weekday",
         y = paste(feature, summary_stat))  
  
  bar_gpu_year = ggplot(dataset, 
                        aes(x = Year, y = get(summary_stat))) + 
    stat_summary(fun = sum, geom = "bar") + 
    labs(tittle = "Barplot", 
         subtitle = paste(feature, "by Year"), 
         x = "Year",
         y = paste(feature, "Sum")) 
  
  bar_gpu_hour = ggplot(dataset, 
                        aes(x = Hour, y = get(summary_stat))) + 
    stat_summary(fun = sum, geom = "bar") + 
    labs(tittle = "Barplot", 
         subtitle = paste(feature, "by Hour"), 
         x = "Hour",
         y = paste(feature, "Sum")) 
  
  ggsave(filename=paste(filepath, feature, "_", summary_stat, "_Barplot.png", sep = ""), 
         plot=grid.arrange(bar_gpu_hour, 
                           bar_gpu_week,
                           bar_gpu_month,
                           bar_gpu_year),
         width = 10, height = 8, dpi = 100)
  
}

# Sumamry: Density plot by month, weekdays, year, hour
# @dataset: a list of summary data of features
# @feature: the feature specified for boxplot
# @summary_stat: specify the summary statistic for boxplot
# @filepath: filepath and name for the plots
# Return: Saved density plots for specified feature across hour, day, week, month, and year
GetDensityplot = function(dataset, feature, summary_stat, filepath){
  dataset$Month = factor(dataset$Month, levels = month.abb)
  dataset$Week = factor(dataset$Week, 
                        levels = c("Mon", "Tue", "Wed",
                                   "Thu", "Fri", "Sat",
                                   "Sun"))
  
  density_gpu_month = ggplot(dataset, 
                             aes(color = Month, x = get(summary_stat))) + 
    geom_density() +
    labs(tittle = "Density plot", 
         subtitle = paste(feature, "by Month"), 
         x = summary_stat,
         y = paste(feature, "Density"))
  
  density_gpu_week = ggplot(dataset, 
                            aes(color = Week, x = get(summary_stat))) + 
    geom_density() +
    labs(tittle = "Density plot", 
         subtitle = paste(feature, "by Weekday"), 
         x = summary_stat,
         y = paste(feature, "Density"))  
  
  density_gpu_year = ggplot(dataset, 
                            aes(color = Year, x = get(summary_stat))) + 
    geom_density() +
    labs(tittle = "Density plot", 
         subtitle = paste(feature, "by Year"), 
         x = summary_stat,
         y = paste(feature, "Density")) 
  
  density_gpu_hour = ggplot(dataset, 
                            aes(color = Hour, x = get(summary_stat))) + 
    geom_density() +
    labs(tittle = "Density plot", 
         subtitle = paste(feature, "by Hour"), 
         x = summary_stat,
         y = paste(feature, "Density")) 
  
  ggsave(filename=paste(filepath, feature, "_", summary_stat, "_densityplot.png", sep = ""), 
         plot=grid.arrange(density_gpu_hour, 
                           density_gpu_week,
                           density_gpu_month,
                           density_gpu_year),
         width = 10, height = 8, dpi = 100)
  
}

# Sumamry: Moving average plot 
# @dataset: a list of summary data of features
# @feature: the feature specified for boxplot
# @summary_stat: specify the summary statistic for boxplot
# @ma_order: scale or vector, which specify the length of moveing average window
# Return: Show moving average plot for specified feature
GetMAplot = function(dataset, feature, summary_stat, ma_order){
  dataset_ma <- dataset
  for (i in 1:length(ma_order)){
    dataset_ma[,paste("ma_",i, sep = "")] <- forecast::ma(dataset[, summary_stat], order = ma_order[i])
  }
  
  ma_gpu_melt <- reshape2::melt(dataset_ma[,c("date", summary_stat, 
                                              paste("ma_",1:length(ma_order), sep = ""))], 
                                id.var='date')
  ma_gpu <- ggplot(ma_gpu_melt) + 
    geom_line(aes(x = as.Date(date), 
                  y = value, color = variable, alpha = 1/length(ma_order))) + 
    guides(alpha = FALSE) +
    labs(tittle = "Moving Average Plot", 
         subtitle = paste(feature, "average per", paste(ma_order*15, collapse = ", "), "mins"), 
         x = "Time",
         y = paste(feature, summary_stat)) 
  return(ma_gpu)
  
}

# Modeling functions ---------------------------
# Sumamry: get the mean and median for each group
# @dataset: dataframe of time series, must include the feature date
# @feature: a certain feature to be summarised by group
# @group: a certain feature contains category to be use 
# Return: Show moving average plot for specified feature
GetAvgBygroup <- function(dataset, feature, group) {
  res <- plyr::ddply(dataset, .(get(group)), 
                     plyr::here(summarize), 
                     mean = mean(get(feature)),
                     median = median(get(feature)))
  colnames(res) <- c(group, "mean", "median")
  return(res)
}

# Sumamry: get the prediction from ARIMA model
# @tuning_set: dataframe of time series, must include the feature gcu_seconds and memory_gib_seconds
# @p: the number of lags for AR model
# @q: the number of lags for MA model
# @forcast_time: the length of next prediction interval
# @predict_set: certain predictors to be added in the model (not for ARIMA model, just be consistent with VAR model).
# @y_feature: certain response variables to be considered
# Return: Prediction of the correponding features
ArimaModel <- function(tuning_set, p = 11, q = 0, 
                       forcast_time = 30, predict_set = NULL,
                       y_feature = NULL){
  fit_gcu <- arima(tuning_set[,"gcu_seconds"], order=c(p, 0, q))
  fit_mem <- arima(tuning_set[,"memory_gib_seconds"], order=c(p, 0, q))
  pred_gcu <- predict(fit_gcu, n.ahead = forcast_time)$pred
  pred_mem <- predict(fit_mem, n.ahead = forcast_time)$pred
  return(list("pred_gcu" = pred_gcu, "pred_mem" = pred_mem))
}


# Sumamry: get the prediction from Bayesian Strutral time series model
# @tuning_set: dataframe of time series, must include the feature gcu_seconds and memory_gib_seconds
# @forcast_time: the length of next prediction interval
# @predict_set: certain predictors to be added in the model (not for ARIMA model, just be consistent with VAR model).
# @y_feature: certain response variables to be considered
# Return: Prediction of the correponding features
BaysianModel <- function(tuning_set,  
                         forcast_time = 30, 
                         y_feature = y_feature,
                         predict_set = NULL) {
  pred_ls <- list()
  for (i in 1:length(y_feature)) {
    ### Run the bsts model
    y <- tuning_set[,y_feature[i]]
    fit <- bsts::AddLocalLinearTrend(list(), y)
    fit <- bsts::AddSeasonal(fit, y, nseasons = 7)
    
    bsts.model <- bsts::bsts(y,
                             state.specification = fit,
                             niter = 500) 
    
    # validation --------------------------------------------------------------------
    burn <- SuggestBurn(0.1, bsts.model)
    pred <- predict(bsts.model,
                    h = forcast_time,
                    burn = burn)
    
    pred_ls[[i]] <- pred$mean
  }
  
  return(list("pred_gcu" = pred_ls[[1]], "pred_mem" = pred_ls[[2]]))
}


## VAR model

# Sumamry: split dataset into training and test set
# @dataset: dataframe
# @interval: the length of the test set
# @hold_day: the number of hold out day to be split
# Return: list of training and test set split
SplitData <- function(dataset, interval = 10, hold_day = 30)  {
  split_ls <- list()
  for (i in 1:floor(hold_day / interval)) {
    cutoff <- (nrow(dataset) - interval*i)
    train_set <- dataset[1:cutoff,]
    test_set <- dataset[(cutoff+1):(cutoff+interval),]
    split_ls[[i]] <- list("train_set" = train_set, "test_set" = test_set)
  }
  return(rev(split_ls))
}

# Sumamry: expand dataset into the form that each selected previous lag is the predictor 
# @dataset: dataframe
# @features: certain features to be expanded
# @lag_num: the number of lags to be expanded
# Return: a dataframe of expanded data
ExpandData <- function(features, dataset, lag_num) {
  X_expand <- list()
  for (i in 1:length(features)) {
    X_expand[[i]] <- t(sapply(1:(nrow(dataset)-lag_num), function(x) dataset[x:(x+lag_num-1),i]))
  }
  X_expand <- do.call("cbind", X_expand)
  return(X_expand)
}

## For VAR model, I have three options:
#1. predict all four features together to get next interval time
#2. predict only gcu and memeory, and use the previous know other features to get next interval time
#3. predict add the current data. If use this option, the default is use the second option


# Sumamry: Add one prediction with same order to previous observations
# @test_set: dataframe of test set
# @x_feature certain predictors to be considered
# @y_feature certain response variables to be considered
# @pred_test: dataframe of prediction set
# @i: the selected observation of pred_test
# Return: a dataframe of expanded data with previous prediction
# NOTE: should be improved
GetNewObs <- function(x_feature, y_feature, 
                      test_set, pred_test, i) {
  new_obs <- c()
  for (j in 1:length(x_feature)) {
    if (x_feature[j] %in% y_feature) {
      for (l in 1:length(y_feature)) {
        if (x_feature[j] == y_feature[l]) {
          new_obs[j] = pred_test[i,l]
        } 
      }
    } else {
      new_obs[j] = test_set[i,x_feature[j]]
    }
  }
  return(new_obs)
}

# Sumamry: Get the next several predictions
# @interval: predict next interval
# @X: input predictor matrix (similar dimensions to train_set)
# @features: names of predictors
# @lag_num: the number of lags to be selected
# @is.together controls whether use multivariate gaussian family
# @current_add: whether we add current features in the data
# @test_set: if current_add is true, then we need specify the test set, or default is null
# @x_pred: specify whether predict all features
# Return: a dataframe of predictions
PredictVar <- function(interval, fit, X, 
                       features, lag_num = 15, 
                       current_add = F, test_set = NULL, 
                       x_pred = F, is.together = T) {
  pred_test <- matrix(0, nrow = interval, ncol = length(features))
  X_tmp <- X
  
  newx_mat <- matrix(0, ncol = ncol(X)*lag_num, nrow = interval+1)
  newx_mat[1,] <- c(X)
  
  
  if (x_pred){
    for ( i in 1:interval) {
      newx <- t(newx_mat[i,])
      if (is.together) {
        pre_val <- predict(fit, newx = newx)
      } else {
        pre_val <- c()
        for (j in 1:length(features)) {
          pre_val[j] <- predict(fit[[j]], newx = newx)
        }
      }
      
      pred_test[i,] <- pre_val
      X_tmp <- rbind(X_tmp, pred_test[i,])
      newx_mat[i+1,] <- c(X_tmp[(1+i):(lag_num+i),]) 
    }
  } else {
    x_feature <- colnames(X)
    x_feature_only <- setdiff(x_feature, features)
    
    if (current_add) {
      newx_mat <- cbind(newx_mat, matrix(0, nrow = nrow(newx_mat), ncol = length(x_feature_only)))
      newx_mat[1, (ncol(newx_mat) - length(x_feature_only) + 1):ncol(newx_mat)] <- as.matrix(test_set[1, x_feature_only])
      test_set <- rbind(test_set, 0)
    } 
    
    for ( i in 1:interval) {
      newx <- t(newx_mat[i,])
      
      if (is.together) {
        pre_val <- predict(fit, newx = newx)
      } else {
        pre_val <- c()
        for (j in 1:length(features)) {
          pre_val[j] <- predict(fit[[j]], newx = newx)
        }
      }
      print(pre_val)
      pred_test[i,] <- pre_val
      
      # fill with column names
      new_obs <- GetNewObs(x_feature = x_feature, y_feature = features, test_set, pred_test, i) 
      X_tmp <- rbind(X_tmp, new_obs)
      
      if (current_add) {
        newx_mat[i+1,] <- c(c(X_tmp[(1+i):(lag_num+i),]), as.matrix(test_set[i+1, x_feature_only]))
      } else {
        newx_mat[i+1,] <- c(X_tmp[(1+i):(lag_num+i),]) 
      }
    }
    
  }
  
  colnames(pred_test) <- features
  
  return(list("pred" = pred_test))#, "error" = error))
}

# Sumamry: Fit the VAR model
# @train_set: the dataframe to train the model
# @lag_num: number of lags to be considered
# @lambda: tuning parameter, either a vector or a scalar 
# @y_feature: the name of response time series only
# @x_feature: the name of time series in the predictor
# @weights: whether use equal weighted or weighted MSE to fit the model
# @is.together controls whether use multivariate gaussian family
# @interval: the prediction interval for forward corss-validation
# @current_add: whether we add current features in the data
# @test_set: if current_add is true, then we need specify the test set, or default is null
# @x_pred: specify whether predict all features
# Return: Predictions for next intervals
VarFit <- function(train_set, lag_num = 15, lambda, 
                   y_feature, x_feature, weights = NULL, 
                   is.together = T, interval = 10, x_pred = F,
                   current_add = F, test_set = NULL) {
  
  n <- nrow(train_set)
  Y <- train_set[(lag_num+1):n, y_feature]
  X <- train_set[, x_feature]
  
  # Re-construct data
  X_expand <- ExpandData(x_feature, X, lag_num)
  X_new <- train_set[(n - lag_num + 1):n, x_feature]
  
  # The third options
  if (current_add) {
    x_feature_only <- setdiff(x_feature, y_feature)
    X_expand <- cbind(X_expand, train_set[(lag_num+1):n, x_feature_only])
  }
  
  
  X <- X_expand
  
  # Fit model
  if (is.together){
    family = "mgaussian"
    fit <- glmnet(X, Y, weights = weights, lambda = lambda, family = family)
    
    # Predict
    pred_test <- PredictVar(interval = interval, 
                            fit = fit, 
                            X = X_new, 
                            features = y_feature, 
                            lag_num = lag_num, 
                            current_add = current_add, 
                            test_set = test_set, 
                            x_pred = x_pred,
                            is.together = is.together)
    pred <- pred_test$pred
  }else{
    family = "gaussian"
    pred <- NULL
    error <- NULL
    fit_ls <- list()
    
    for (i in 1:length(y_feature)) {
      if (is.list(lambda)) {
        lambda_sel <- lambda[[i]]
      } else {
        lambda_sel <- lambda
      }
      fit <- glmnet(X, Y[,i], weights = weights, lambda = lambda_sel, family = family)
      fit_ls[[i]] <- fit
    }
    # Predict
    pred_test <- PredictVar(interval = interval, 
                            fit = fit_ls, 
                            X = X_new, 
                            features = y_feature, 
                            lag_num = lag_num, 
                            current_add = current_add, 
                            test_set = test_set, 
                            x_pred = x_pred, is.together = is.together)
    pred <- pred_test$pred
  }
  
  
  return(list("pred" = pred))
}


# Sumamry: Forward validation for select tuning parameters
# @dataset: the dataframe to train the model
# @tuning_interval: the length of interval of test set in parameter tuning procedure
# @lambda: tuning parameter, either a vector or a scalar 
# @hold_day: the number of hold out day to tune the model
# @lag_num: number of lags to be considered
# @x_feature: the name of time series in the predictor
# @y_feature: the name of response time series only
# @is.together controls whether use multivariate gaussian family
# @weight_type: whether use equal weighted or weighted MSE to fit the model
# @forcast_time: the prediction interval for forward corss-validation
# @predict_set: the test set to tune the parameter
# @x_pred: specify whether predict all features
# @current_add: whether we add current features in the data
# @tuning_parameter: specify the tuning parameter
# Return: Predictions for next intervals and the tuning parameter
VarModel <- function(dataset, tuning_interval = 10, lambda = seq(0.0001,0.005, length.out = 20), 
                     hold_day = 30, lag_num = 15, x_feature = x_feature, y_feature = y_feature,
                     is.together = T, weight_type = "equal", forcast_time = forcast_time,
                     predict_set = validation_set_new, x_pred = F, current_add = T, 
                     tuning_parameter = NULL, tune_type = "moving") {
  # Normalize data
  data_mean <- apply(dataset[, x_feature], 2, mean)
  data_sd <- apply(dataset[, x_feature], 2, sd)
  data_norm <- scale(dataset[, x_feature], center = T, scale = T)
  
  x_feature_only <- setdiff(x_feature, y_feature)
  predict_set <- as.data.frame(sapply(x_feature_only, function(x) (predict_set[,x] - data_mean[x])/data_sd[x]))
  colnames(predict_set) <- x_feature_only
  y_feature_org <- y_feature
  
  if (x_pred){
    y_feature <- x_feature
  }
  
  if (is.null(tuning_parameter)) {
    # Split data
    data_split <- SplitData(data_norm, interval = tuning_interval, hold_day = hold_day)
    
    # Parameter tuning
    if (is.together) {
      mse = matrix(0, nrow = length(data_split), ncol = length(lambda))
    } else {
      mse = lapply(1:length(y_feature), function(x) matrix(0, nrow = length(data_split), ncol = length(lambda)))
    }
    
    
    for (i in 1:length(data_split)) {
      
      if (tune_type == "expand") {
        train_set_start = 1
      }
      if (tune_type == "moving") {
        train_set_start = 1 + (i - 1)*tuning_interval
      }
      train_set <- data_split[[i]]$train_set[train_set_start:nrow(data_split[[i]]$train_set), ]
      test_set <- data_split[[i]]$test_set
      
      if (weight_type == "equal") {
        weights <- rep(1, nrow(train_set)-lag_num)
      }else{
        weights <- (1 : (nrow(train_set)-lag_num)) / (nrow(train_set)-lag_num)
      }
      
      for (j in 1:length(lambda)) {
        res <- VarFit(train_set = train_set, 
                      lag_num = lag_num, 
                      lambda = lambda[j], 
                      y_feature = y_feature, 
                      x_feature = x_feature, 
                      weights = weights, 
                      is.together = is.together, 
                      interval = tuning_interval,
                      x_pred = x_pred,
                      current_add = current_add, 
                      test_set = test_set)
        if (is.together) {
          mse[i,j] <- mean(unlist((test_set[,y_feature_org] - res$pred[,y_feature_org])^2))
        } else {
          for (k in 1:length(mse)) {
            mse[[k]][i,j] <- mean((test_set[,y_feature[k]] - res$pred[,y_feature[k]])^2)
          }
        }
        
        
      }
    }
    
    if (is.together) {
      mse_cv <- apply(mse, 2, mean) 
      plot(mse_cv, type = "l", col = 2)
      lambda_min <- lambda[which.min(mse_cv)]
    } else {
      mse_cv <- lapply(mse, function(x) {
        tmp <- apply(x, 2, mean)
        plot(tmp, type = "l", col = 2)
        return(tmp)
      })
      lambda_min <- lapply(mse_cv, function(x) lambda[which.min(x)])
    }
    
    
  } else {
    lambda_min <- tuning_parameter
  }
  
  
  # Predict
  if (weight_type == "equal") {
    weights <- rep(1, nrow(data_norm)-lag_num)
  }else{
    weights <- (1 : (nrow(data_norm)-lag_num)) / (nrow(data_norm)-lag_num)
  }
  
  
  
  pred <- VarFit(train_set = data_norm, 
                 lag_num = lag_num, 
                 lambda = lambda_min, 
                 y_feature = y_feature, 
                 x_feature = x_feature, 
                 weights = weights, 
                 is.together = is.together, 
                 interval = forcast_time,
                 x_pred = x_pred,
                 current_add = current_add, 
                 test_set = predict_set)
  
  data_mean <- data_mean[y_feature]
  data_sd <- data_sd[y_feature]
  
  pred_normback <- sapply(1:length(y_feature), function(x) (pred$pred[,y_feature[x]] * data_sd[x]) + data_mean[x])
  colnames(pred_normback) <- y_feature
  print(paste("lambda_min:",lambda_min))
  return(list("pred_gcu" = pred_normback[,"gcu_seconds" ], "pred_mem" = pred_normback[,"memory_gib_seconds"], "tuning_parameter" = lambda_min))
}


# Validate---------------------------
# Sumamry: Define the transformation method
# @dataset: dataframe
# @initial_data: contains the initial values before the transformation
# @features: name of the features we want to transform
# @tranform_type: type of transformation
# Return: transform back the transformed data
Backtransform <- function(dataset, 
                          initial_data = initial_data,
                          features = c("gcu_seconds", "memory_gib_seconds"),
                          tranform_type = "log_diff") {
  if (tranform_type == "log_diff") {
    data_org <- as.data.frame(matrix(0, ncol = length(features), nrow = (nrow(dataset)+1)))
    colnames(data_org) <- features
    data_org[1,] <- log(initial_data[,features])
    
    for (i in 1:nrow(dataset)) {
      data_org[i+1,] <- dataset[i,features] + data_org[i,]
    }
    res <- exp(data_org)
    res <- res[-1,]
  }
  return(res)
}


# Sumamry: Create a validation framework to plug in models
# @validate_type: the type of validation to call
# @model: model want to call
# @tuning_set: the dataframe of the tuning set
# @validation_set: data to do model selection
# @validation_interval: validate windows
# @metric: metric selected to evaluate the model performance
# @is.plot: whether need to show the plot
# @is.origin: whether show the results transformed back on the origin data
# @transform.fun: the back transformation function
# @initial_data: the initial data used to transfer back
# @is.tuned: whether model need to tune the parameters
# Return the results for gcu and memeory
ValidateModel <- function(validate_type = "expand",
                          model = ArimaModel, 
                          tuning_set = tuning_set, 
                          validation_set = validation_set, 
                          validation_interval = 7, 
                          metric = "MSE", 
                          is.plot = TRUE, 
                          is.origin = TRUE, 
                          transform.fun = Backtransform, 
                          initial_data = initial_data,
                          is.tuning = T,
                          ...
){
  
  # validation_times = Prediction time - 1
  validation_times <- floor(nrow(validation_set) / validation_interval-1)
  
  res <- list()
  error <- list("gcu_err" = NULL, "mem_err" = NULL)
  features <- c("gcu_seconds", "memory_gib_seconds")
  
  # If need to transform back to the original data, transform back.
  if (is.origin) {
    data_origin_all <- Backtransform(rbind(tuning_set, validation_set),
                                     initial_data = initial_data,
                                     features = features,
                                     tranform_type = "log_diff")
    data_origin_all$date = rbind(tuning_set, validation_set)$date
    validation_set_org <- data_origin_all[data_origin_all$date > tuning_set[nrow(tuning_set), "date"], ]
    data_origin_pred_ls <- list()
  }
  
  # Validate model
  tuning_parameter <- NULL
  
  
  
  for (i in 0:validation_times) {
    if (validate_type == "expand") {
      tuning_set_start = 1
    }
    if (validate_type == "moving") {
      tuning_set_start = 1 + i*validation_interval
    }
    index_sel <- 0:(i*validation_interval)
    tune_set_new <- rbind(tuning_set, validation_set[index_sel, ])
    
    validate_date_start <- tune_set_new$date[nrow(tune_set_new)]
    validation_set_new <- validation_set[validation_set$date > validate_date_start,][1:validation_interval,]
    
    if (is.null(tuning_parameter)) {
      res[[i+1]] <- model(tune_set_new[(tuning_set_start):nrow(tune_set_new), ],
                          forcast_time = validation_interval,
                          predict_set = validation_set_new,
                          y_feature = features,
                          ...)
      if (is.tuning) {
        tuning_parameter <- res[[i+1]]$tuning_parameter
      }
    } else {
      res[[i+1]] <- model(tune_set_new[(tuning_set_start):nrow(tune_set_new), ],
                          forcast_time = validation_interval,
                          predict_set = validation_set_new,
                          y_feature = features,
                          tuning_parameter = tuning_parameter,
                          ...)
    }
    
    
    if (is.origin) {
      validation_set_pred <- as.data.frame(rbind(as.matrix(tune_set_new[,features]), cbind(res[[i+1]]$pred_gcu, res[[i+1]]$pred_mem)))
      colnames(validation_set_pred) <- features
      data_origin_pred <- transform.fun(validation_set_pred,
                                        initial_data = initial_data,
                                        features = features,
                                        tranform_type = "log_diff")
      
      data_origin_pred <- data_origin_pred[(nrow(data_origin_pred) - validation_interval + 1): nrow(data_origin_pred), ]
      validation_set_new_org <- validation_set_org[validation_set_org$date > validate_date_start,][1:validation_interval,]
      error$gcu_err[[i+1]] <- data_origin_pred$gcu_seconds - 
        validation_set_new_org$gcu_seconds
      error$mem_err[[i+1]] <- data_origin_pred$memory_gib_seconds - 
        validation_set_new_org$memory_gib_seconds
      data_origin_pred_ls[[i+1]] <- data_origin_pred
      
    } else {
      error$gcu_err[[i+1]] <- res[[i+1]]$pred_gcu - validation_set_new$gcu_seconds
      error$mem_err[[i+1]] <- res[[i+1]]$pred_mem - validation_set_new$memory_gib_seconds
    }
  }
  
  
  eval_res <- list("gcu" = NULL, "mem" = NULL)
  
  if (metric == "MSE") {
    eval_res$gcu <- mean(unlist(error$gcu_err)^2)
    eval_res$mem <- mean(unlist(error$mem_err)^2)
  }
  
  
  
  if (is.plot == TRUE) {
    pred_transform <- lapply(res, function(x) cbind(x$pred_gcu, x$pred_mem))
    pred_transform <- as.data.frame(do.call("rbind", pred_transform))
    colnames(pred_transform) <- features
    data_transform_all <- rbind(tuning_set, validation_set)
    xlabel <- data_transform_all$date
    
    if (is.origin) {
      par(mfrow=c(2,2))
      plot_data <- do.call("rbind", data_origin_pred_ls)
      tune_set_org <- data_origin_all[data_origin_all$date <= tuning_set[nrow(tuning_set), "date"], ]
      plot_data <- rbind(tune_set_org[, features], plot_data)
      
      for (j in features) {
        # plot origin
        # red line is the model prediction
        plot(xlabel[1:nrow(plot_data)], plot_data[, j], type = "l", col = 2, ylab = j, xlab = "Time", main = "Original") 
        lines(xlabel,data_origin_all[, j], col = 1)
        abline(v = xlabel[nrow(tuning_set)], col = 3, lty = 2)
        
        # plot transformed data
        plot(xlabel[1:nrow(plot_data)], c(tuning_set[,j], pred_transform[, j]), type = "l", col = 2, ylab = j, xlab = "Time", main = "Transformed") 
        lines(xlabel,data_transform_all[, j], col = 1)
        abline(v = xlabel[nrow(tuning_set)], col = 3, lty = 2)
        
      }
      
    }else {
      par(mfrow=c(2,1))
      # plot transformed data
      for (j in features) {
        plot(xlabel[1:nrow(plot_data)], c(tuning_set[,j], pred_transform[, j]), type = "l", col = 2, ylab = j, xlab = "Time", main = "Transformed") 
        lines(xlabel,data_transform_all[, j], col = 1)
        abline(v = xlabel[nrow(tuning_set)], col = 3, lty = 2)
      }
    }
    title(paste(as.character(substitute(model)), "with", validation_interval, "days validation"), line = -1, outer = TRUE)
  }
  return(list("MSE" = eval_res, "prediction" = plot_data))
  
}

# Result Table---------------------------
# Summary tables for different VAR settings
# VAR model accross different prediction time with different tuning interval
# @tuning_set: the dataframe of the tuning set
# @lag_num: number of lags to be considered
# @is.together controls whether use multivariate gaussian family
# @weight_type: whether use equal weighted or weighted MSE to fit the model
# Return: MSE for all features, and plots for the fitted value
GetTunValTable <- function(tuning_set, is.together, lag_num = 18, weight_type = "equal") {
  validation_interval <- c(3, 7, 14, 30)
  hold_day <- c(30, 60)
  mse_var <- matrix(0, nrow = length(hold_day)*2, ncol = length(validation_interval))
  initial_data <- info_table_1day_clean_agg[as.Date(info_table_1day_clean_agg$date) == (as.Date(tuning_set[1,]$date)-1), ]
  x_feature <- c("duration", "instances", "gcu_seconds", "memory_gib_seconds")
  for (i in 1:length(validation_interval)) {
    for (j in 1:length(hold_day)) {
      validation_interval_i <- validation_interval[i]
      hold_day_j <- hold_day[j]
      var_res_together <- ValidateModel(validate_type = "moving",
                                        model = VarModel, 
                                        tuning_set = tuning_set, 
                                        validation_set = validation_set, 
                                        validation_interval = validation_interval_i,
                                        metric = "MSE", 
                                        is.plot = TRUE, 
                                        is.origin = T, 
                                        transform.fun = Backtransform, 
                                        initial_data = initial_data,
                                        tuning_interval = 10, 
                                        lambda = seq(0.00025,0.1,0.0005), 
                                        hold_day = hold_day_j, 
                                        lag_num = lag_num, 
                                        x_feature = x_feature, 
                                        is.together = is.together, 
                                        weight_type = weight_type,
                                        x_pred = T, 
                                        current_add = F,
                                        is.tuning = T
      )
      mse_var[2*j-1,i] <- var_res_together$MSE$gcu
      mse_var[2*j,i] <- var_res_together$MSE$mem
    }
  }
  return(mse_var)
  
}

# Summary tables for model comparison
# @model: model want to call
# @tuning_set: the dataframe of the tuning set
# @is.tuned: whether model need to tune the parameters
# Return: MSE for all features, and plots for the fitted value
GetModelValTable <- function(model, is.tuning, ...) {
  validation_interval <- c(3, 7, 14, 30)
  mse_var <- matrix(0, nrow = 2, ncol = length(validation_interval))
  cutoff <- 366
  tuning_set2 <- tuning_set[cutoff:nrow(tuning_set),]
  initial_data <- info_table_1day_clean_agg[as.Date(info_table_1day_clean_agg$date) == (as.Date(tuning_set2[1,]$date)-1), ]
  for (i in 1:length(validation_interval)) {
    validation_interval_i <- validation_interval[i]
    var_res_together <- ValidateModel(validate_type = "uniform",
                                      model = model,  
                                      tuning_set = tuning_set2, 
                                      validation_set = validation_set, 
                                      validation_interval = validation_interval_i,
                                      metric = "MSE", 
                                      is.plot = TRUE, 
                                      is.origin = T, 
                                      transform.fun = Backtransform, 
                                      initial_data = initial_data,
                                      is.tuning = is.tuning,
                                      ...
                                      
    )
    mse_var[1,i] <- var_res_together$MSE$gcu
    mse_var[2,i] <- var_res_together$MSE$mem
  }
  return(mse_var)
}

