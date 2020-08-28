
# install.packages("tseries")
# Import packages ---------------------------
library(plyr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(xtable)
library(gridExtra)
library(forecast)
library(reshape2)
library(tseries)
library(knitr)

# Remove points with "-" in the cell
# @dataset is the vector 
# Return True or False
IsDash <- function(dataset) {
  string_split <- strsplit(as.character(dataset), split = "")
  res <- lapply(string_split, function(x) "-" %in% x)
  return(unlist(res))
}


# Create day, week, month, year variables to summarize
# @dataset is the matrix including date column
# Return the matrix combined with the day, week, month, year information
GetTimeInfo <- function(dataset){
  time_stamp <- dataset$date
  time_info <- data.frame(cbind(format(time_stamp, "%b"),
                                format(time_stamp, "%a"),
                                format(time_stamp, "%d"),
                                format(time_stamp, "%y")
  ))
  colnames(time_info) <- c("Month", "Week", "Day", "Year")
  time_info_merge = cbind(time_info, dataset)
  return(time_info_merge)
}

# Pick up cells with date after 2018-03-28
# @dataset is the matrix including date column with POSIXct type
# @group is the string of certain group
# @certain_date is the string form of date we want to check
# Return the group name and True or False
IsAfterDate <- function(dataset, group, certain_date) {
  idx = ddply(dataset, .(get(group)), summarize, res = sort(date)[1] > 
                as.POSIXct(certain_date))
  return(idx)
}

de_seasonal <- function(dataset, features = c("gcu_seconds", "memory_gib_seconds"), season, type = "variance") {
  res <- dataset
  if (type == "diff") {
    res[(1+season):nrow(dataset), features] <- dataset[(1+season):nrow(dataset), features] - 
      dataset[1:(nrow(dataset)-season), features]
  }
  if (type == "variance") {
    mean_season <- lapply(1:season, function(x) {
      apply(dataset[seq(x, nrow(dataset), season), features], 2, mean)
    })
    res[(1+season):nrow(dataset), features] <- dataset[(1+season):nrow(dataset), features] - 
      apply(dataset[1:(nrow(dataset)-season), features], 2, mean)
  }
  return(res)
}



# Visulazation ---------------------------
# General plot by each second

# Line plot across times
# @dataset: original time series
# @group: group by certain group
# @filepath: path to save the image
# Return pictures saved in specified filepath
GetLineplot = function(dataset, group, filepath) {
  gg_gcu = ggplot(dataset, 
                  aes(x = as.Date(date), y = gcu_seconds, col = get(group))) + 
    geom_line() + 
    xlab("") + theme(legend.position="none")
  
  gg_memory = ggplot(dataset, 
                     aes(x = as.Date(date), y = memory_gib_seconds, col = get(group))) + 
    geom_line() + 
    xlab("") +  theme(legend.position="none")
  
  ggsave(filename = paste(filepath,"gcu_memory_linegraph.png", sep = ""), 
         plot = grid.arrange(gg_gcu, gg_memory),
         width = 10, height = 8, dpi = 100)
}

# Box plot by month, weekdays, year
# @dataset: original time series
# @group: boxplot of certain group
# @feature: the feature we want to check
# @filepath: path to save the image
# Return pictures saved in specified filepath
GetBoxplot = function(dataset, group, feature, filepath){
  dataset$Month = factor(dataset$Month, levels = month.abb)
  dataset$Week = factor(dataset$Week, 
                        levels = c("Mon", "Tue", "Wed",
                                   "Thu", "Fri", "Sat",
                                   "Sun"))
  dataset <- dataset[, c(group, feature, "Month", "Week", "Year")]
  box_gpu_month <- ggplot(dataset, 
                          aes(x = Month, y = get(feature))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature, "by Month"), 
         x = "Month",
         y = paste(feature))
  
  box_gpu_week <- ggplot(dataset, 
                         aes(x = Week, y  = get(feature))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature, "by Weekday"), 
         x = "Weekday",
         y = paste(feature))  
  
  box_gpu_year <- ggplot(dataset, 
                         aes(x = Year, y = get(feature))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature, "by Year"), 
         x = "Year",
         y = paste(feature)) 
  
  ggsave(filename=paste(filepath, feature, "_boxplot.png", sep = ""), 
         plot=grid.arrange(box_gpu_week,
                           box_gpu_month,
                           box_gpu_year),
         width = 10, height = 8, dpi = 100)
  
}

# Density plot by month, weekdays, year
# @dataset: original time series
# @group: group by certain group
# @feature: the feature we want to check
# @filepath: path to save the image
# Return pictures saved in specified filepath

GetDensityplot = function(dataset, group, feature, filepath){
  dataset$Month = factor(dataset$Month, levels = month.abb)
  dataset$Week = factor(dataset$Week, 
                        levels = c("Mon", "Tue", "Wed",
                                   "Thu", "Fri", "Sat",
                                   "Sun"))
  
  density_month = ggplot(dataset, 
                         aes(x = get(feature), col = get(group))) + 
    facet_wrap(~ Month) +
    geom_density() + 
    theme(legend.position="none") +
    labs(tittle = "Density plot", 
         subtitle = paste(feature, "by Month"), 
         x = feature,
         y = "Density")
  
  density_week = ggplot(dataset, 
                        aes(x = get(feature), col = get(group))) + 
    facet_wrap(~ Week) +
    geom_density() + 
    theme(legend.position="none") +
    labs(tittle = "Density plot", 
         subtitle = paste(feature, "by Week"), 
         x = feature,
         y = "Density")
  
  density_year = ggplot(dataset, 
                        aes(x = get(feature), col = get(group))) + 
    facet_wrap(~ Year) +
    geom_density() + 
    theme(legend.position="none") +
    labs(tittle = "Density plot", 
         subtitle = paste(feature, "by Year"), 
         x = feature,
         y = "Density")
  
  
  ggsave(filename=paste(filepath, feature, "_densityplot.png", sep = ""), 
         plot=grid.arrange(density_week,
                           density_month,
                           density_year),
         width = 10, height = 8, dpi = 100)
  
}

# Moving average plot 
# @dataset: original time series
# @group: group by certain group
# @feature: the feature we want to check
# @filepath: path to save the image
# @ma_order a scale
# Return pictures saved in specified filepath
GetMAplot = function(dataset, feature, group, ma_order, filepath){
  dataset_ma <- dataset
  dataset_ma[,"ma_order"] <- forecast::ma(dataset[, feature], order = ma_order)
  
  ma_gpu_melt <- reshape2::melt(dataset_ma[,c("date", group,
                                              "ma_order")], 
                                id.var=c('date', group))
  ma_gpu <- ggplot(ma_gpu_melt) + 
    geom_line(aes(x = as.Date(date), 
                  y = value, color = get(group), alpha = 1/length(group))) + 
    guides(alpha = FALSE) +
    theme(legend.position="none") +
    labs(tittle = "Moving Average Plot", 
         subtitle = paste(feature, "average per", ma_order, "days"), 
         x = "Time",
         y = feature) 
  ggsave(filename=paste(filepath, feature, "_maplot.png", sep = ""), 
         plot=grid.arrange(ma_gpu),
         width = 10, height = 8, dpi = 100)
  
}

# Change data to wide
# @dataset: dataframe containing time series to be clustered.
# @key: name of column containing the new column names.
# @obs: name of the row containing the new row names.
# @features_name: name of column containing values.

GetClusterPrep <- function(dataset, features_name, obs, key) {
  dataset_res <- unique(dataset[,obs])
  for (i in features_name) {
    tmp <- reshape2::dcast(dataset, get(obs) ~ get(key), value.var = i, fill = NaN)
    dataset_res <- cbind(dataset_res, tmp[,-1])
  }
  return(dataset_res)
}



# Modeling---------------------------
## Bechmark model
# @tuning_set: the dataframe to fit the model.
# @forcast_time: the number of steps ahead at which to predict.
# @predict_set: the dataframe contains the time series to help predict (not used in this model, just to make all models consistent)..
# @y_feature: the name of response time series only.
MedianModel <- function(tuning_set,  
                       forcast_time = 30, predict_set = NULL,
                       y_feature = NULL){
  fit_gcu <- median(tuning_set[,"gcu_seconds"])
  fit_mem <- median(tuning_set[,"memory_gib_seconds"])
  pred_gcu <- rep(fit_gcu, forcast_time)
  pred_mem <- rep(fit_mem, forcast_time)
  return(list("pred_gcu" = pred_gcu, "pred_mem" = pred_mem))
}

## Arima model
ArimaModel <- function(tuning_set, p = 11, q = 0, 
                       forcast_time = 30, predict_set = NULL,
                       y_feature = NULL){
  fit_gcu <- arima(tuning_set[,"gcu_seconds"], order=c(p, 0, q))
  fit_mem <- arima(tuning_set[,"memory_gib_seconds"], order=c(p, 0, q))
  pred_gcu <- predict(fit_gcu, n.ahead = forcast_time)$pred
  pred_mem <- predict(fit_gcu, n.ahead = forcast_time)$pred
  return(list("pred_gcu" = pred_gcu, "pred_mem" = pred_mem))
}


## VAR model

# Split data
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


# Expand data
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


# Add one prediction with same order to previous observations
# @test_set is used to give features for current or previous value
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

# Predict next interval time
# @interval: predict next interval
# @X: input predictor matrix (similar dimensions to train_set)
# @features: names of predictors
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

# Return predictions for next intervals

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



VarModel <- function(dataset, tuning_interval = 10, lambda = seq(0.0001,0.005, length.out = 20), 
                     hold_day = 30, lag_num = 15, x_feature = x_feature, y_feature = y_feature,
                     is.together = T, weight_type = "equal", forcast_time = forcast_time,
                     predict_set = validation_set_new, x_pred = T, current_add = F, 
                     tuning_parameter = NULL) {
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
      train_set <- data_split[[i]]$train_set
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
    weights <- (1 : (nrow(data_norm)-lag_num)) 
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
# Define the transformation method
# @dataset: dataframe
# @initial_data: contains the initial values before the transformation
# @features: name of the features we want to transform
# @tranform_type: type of transformation
# @season: the seasonality want to be added back
Backtransform <- function(dataset, 
                          initial_data = initial_data,
                          features = c("gcu_seconds", "memory_gib_seconds"),
                          tranform_type = "log_diff", season = NULL,
                          missingdata = NULL) {
  
  if (!is.null(season)) {
    for (j in features) {
      for (i in (season+1):nrow(dataset)) {
        dataset[i,j] <- dataset[i-season, j] + dataset[i, j] 
      }
    }
  }
  
  if (!is.null(missingdata)) {
    for (i in features) {
      miss_ind <- missingdata$index[missingdata$features == i]
      miss_ind <- miss_ind[miss_ind < nrow(dataset)]
      dataset[miss_ind, i] <- missingdata$original_data[missingdata$index %in% miss_ind]
    }
  }
  
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



# Create a validation framework to plug in models
# @model: model want to call
# @hold_day: date to split the tuning set
# @validation_set: data to do model selection
# @validation_interval: validate windows
# @is.tuned: whether model need to tune the parameters
# @metric: metric to be selected
# @is.plot: whether need to show the plot
# @is.origin: whether show the results transformed back on the origin data
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
                          season = 7,
                          missingdata = missingdata,
                          ...
){
  
  # validation_times = Prediction time - 1
  validation_times <- floor(nrow(validation_set) / validation_interval-1)
  
  res <- list()
  error <- list("gcu_err" = NULL, "mem_err" = NULL)
  features <- c("gcu_seconds", "memory_gib_seconds")
  
  # If need to transform back to the original data, transform back.
  if (is.origin) {
    data_origin_all <- transform.fun(rbind(tuning_set, validation_set),
                                     initial_data = initial_data,
                                     features = features,
                                     tranform_type = "log_diff", 
                                     season = season,
                                     missingdata = missingdata)
    # data_origin_all <- Backtransform(rbind(tuning_set, validation_set),
    #                                  initial_data = initial_data,
    #                                  features = features,
    #                                  tranform_type = "log_diff", season = 7)
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
      res[[i+1]] <- model(tune_set_new[(season+tuning_set_start):nrow(tune_set_new), ],
                          forcast_time = validation_interval,
                          predict_set = validation_set_new,
                          y_feature = features,
                          ...)
      # tune_set_new2 <- tune_set_new[(season+tuning_set_start):nrow(tune_set_new), ]
      # res[[i+1]] <- VarModel(dataset = tune_set_new2, tuning_interval = 10,
      #                        lambda = seq(0.0001, 0.1, length.out = 20),
      #                        hold_day = 30, lag_num = p, x_feature = x_feature,
      #                        y_feature = features,
      #                        is.together = F, weight_type = "equal",
      #                        forcast_time = validation_interval,
      #                        predict_set = validation_set_new,
      #                        x_pred = T, current_add = F)
      
      if (is.tuning) {
        tuning_parameter <- res[[i+1]]$tuning_parameter
      }
    } else {
      res[[i+1]] <- model(tune_set_new[(season+tuning_set_start):nrow(tune_set_new), ],
                          forcast_time = validation_interval,
                          predict_set = validation_set_new,
                          y_feature = features,
                          tuning_parameter = tuning_parameter,
                          ...)
      # res[[i+1]] <- VarModel(dataset = tune_set_new, tuning_interval = 10,
      #                        lambda = seq(0.01, 1, length.out = 20),
      #                        hold_day = 30, lag_num = p, x_feature = x_feature,
      #                        y_feature = features,
      #                        is.together = F, weight_type = "equal",
      #                        forcast_time = validation_interval,
      #                        predict_set = validation_set_new,
      #                        x_pred = T, current_add = F, tuning_parameter = tuning_parameter)
    }
    
    
    if (is.origin) {
      validation_set_pred <- as.data.frame(rbind(as.matrix(tune_set_new[,features]), cbind(res[[i+1]]$pred_gcu, res[[i+1]]$pred_mem)))
      colnames(validation_set_pred) <- features
      data_origin_pred <- transform.fun(validation_set_pred,
                                        initial_data = initial_data,
                                        features = features,
                                        tranform_type = "log_diff", 
                                        season = season, 
                                        missingdata = missingdata)
      
      # data_origin_pred <- Backtransform(validation_set_pred,
      #                                  initial_data = initial_data,
      #                                  features = features,
      #                                  tranform_type = "log_diff", season = season)
      
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
        # axis(side=1, at=xtick, labels = FALSE)
        
        # plot transformed data
        plot(xlabel[1:nrow(plot_data)], c(tuning_set[,j], pred_transform[, j]), type = "l", col = 2, ylab = j, xlab = "Time", main = "Transformed") 
        lines(xlabel,data_transform_all[, j], col = 1)
        # abline(v = nrow(plot_data) - (1:(validation_times+1)*validation_interval), col = 3, lty = 2)
        abline(v = xlabel[nrow(tuning_set)], col = 3, lty = 2)
        
      }
      
    }else {
      par(mfrow=c(2,1))
      # plot transformed data
      for (j in features) {
        plot(c(tuning_set[,j], pred_transform[, j]), type = "l", col = 2, ylab = j, xlab = "Time", main = "Transformed") 
        lines(data_transform_all[, j], col = 1)
        abline(v = nrow(plot_data) - (1:(validation_times+1)*validation_interval), col = 3, lty = 2)
      }
    }
    title(paste(as.character(substitute(model)), "with", validation_interval, "days validation"), line = -1, outer = TRUE)
  }
  return(list("MSE" = eval_res, "prediction" = plot_data))
  
}


# Baysian Model
BaysianModel <- function(tuning_set,  
                         forcast_time = 30, 
                         y_feature = y_feature,
                         predict_set = NULL) {
  pred_ls <- list()
  for (i in 1:length(y_feature)) {
    ### Run the bsts model
    y <- tuning_set[,y_feature[i]]
    fit <- bsts::AddLocalLinearTrend(list(), y)
    fit <- bsts::AddSeasonal(fit, y, nseasons = 52)
    
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


# Result Table---------------------------
GetModelValTable <- function(model, is.tuning, ...) {
  validation_interval <- c(3, 7, 14)
  mse_var <- matrix(0, nrow = 2, ncol = length(validation_interval))
  cutoff <- 7
  season <- 7
  # tuning_set2 <- rbind(data_agg_sum_trans_log[(cutoff - season + 1):cutoff,], 
  #                      tuning_set[cutoff:nrow(tuning_set),])
  tuning_set2 <- tuning_set
  initial_data <- data_agg_sum[as.Date(data_agg_sum$date) == (as.Date(tuning_set2[1,]$date)-1), ]
  print(initial_data)
  for (i in 1:length(validation_interval)) {
    validation_interval_i <- validation_interval[i]
    var_res_together <- ValidateModel(validate_type = "moving",
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
                                      missingdata = missingdata,
                                      ...
                                      
    )
    mse_var[1,i] <- var_res_together$MSE$gcu
    mse_var[2,i] <- var_res_together$MSE$mem
  }
  return(mse_var)
}

# Summary tables
# VAR model accross different prediction time with different tuning interval
GetTunValTable <- function(tuning_set, is.together, lag_num = 28, weight_type = "equal") {
  validation_interval <- c(3, 7, 14)
  hold_day <- 14
  mse_var <- matrix(0, nrow = length(hold_day)*2, ncol = length(validation_interval))
  
  initial_data <- data_agg_sum[as.Date(data_agg_sum$date) == (as.Date(tuning_set[1,]$date)-1), ]
  x_feature <- c("gcu_seconds", "memory_gib_seconds")
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
                                        tuning_interval = 7, 
                                        lambda = 10^seq(-4,0.2,0.01), 
                                        hold_day = hold_day_j, 
                                        lag_num = lag_num, 
                                        x_feature = x_feature, 
                                        is.together = is.together, 
                                        weight_type = weight_type,
                                        x_pred = T, 
                                        current_add = F,
                                        is.tuning = T,
                                        season = 7,
                                        missingdata = missingdata
      )
      mse_var[2*j-1,i] <- var_res_together$MSE$gcu
      mse_var[2*j,i] <- var_res_together$MSE$mem
    }
  }
  return(mse_var)
  
}

# Zoom in the plots
# @dataset: dataframe containing the prediction for original data
# @actual_set: contains the original values before the transformation
# @interval: interval of the zoom in part
# @breakline: add the vertical line indicating the start date of the prediction
GetZoomIn <- function(dataset, actual_set, interval, breakline) {
  par(mfrow=c(2,1))
  for (i in features_name) {
    plot(dataset$date[interval], dataset[interval, i], type = "l", 
         col = 2, ylab = i, xlab = "Time", main = "Original")
    lines(actual_set$date[interval], 
          actual_set[interval, i], col = 1)
    abline(v = breakline, col = 3, lty = 2)
  }
  
}
