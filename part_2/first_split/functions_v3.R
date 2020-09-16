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

# Exploratory analysis functions ---------------------------
# Summary: Create day, week, month, year variables.
# @dataset: dataframe with features, must include the feature `data`, which contains time information
# Return: dataframe with day, week, month, and year variables
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

# Summary: Pick up cells with date after certain date
# @dataset: the matrix including date column with POSIXct type
# @group: the vector of feature names
# @certain_date is the string form of date we want to check
# Return: the group name and True or False
IsAfterDate <- function(dataset, group, certain_date) {
  idx = ddply(dataset, .(get(group)), summarize, res = sort(date)[1] > 
                as.Date(certain_date))
  return(idx)
}

# Summary: De-season the time series
# @dataset: the matrix or dataframe
# @features: a vector of feature name
# @season: the length of seasonality to de-season
# @type: the type of de-seasoning procedure
# Return: the dataframe of de-seasoned data
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

    for (i in 1:season) {
      res[seq(i, nrow(dataset), season), features] <- dataset[seq(i, nrow(dataset), season), features] -
        mean_season[[i]]
    }

  }
  return(res)
}


# Visulazation ---------------------------
# Summary: Line plot across times
# @dataset: original time series
# @group: group by certain group
# @filepath: path to save the image
# Return: pictures saved in specified filepath
GetLineplot = function(dataset, group, filepath) {
  gg_gcu = ggplot(dataset, 
                  aes(x = as.Date(date), y = gcu_seconds, col = get(group))) + 
    geom_line() + 
    xlab("Time") +
    ylab("GCU-Seconds") + 
    theme(legend.position="none")
  
  gg_memory = ggplot(dataset, 
                     aes(x = as.Date(date), y = memory_gib_seconds, col = get(group))) + 
    geom_line() + 
    xlab("Time") +
    ylab("RAM (GiB) - Seconds") +
    theme(legend.position="none")
  
  ggsave(filename = paste(filepath,"gcu_memory_linegraph.png", sep = ""), 
         plot = grid.arrange(gg_gcu, gg_memory),
         width = 10, height = 8, dpi = 100)
}

# Summary: Box plot by month, weekdays, year.
# @dataset: original time series.
# @group: boxplot of certain group.
# @feature: the feature we want to check.
# @filepath: path to save the image.
# @feature_lab: y-label of the plots.
# Return pictures saved in specified filepath.
GetBoxplot = function(dataset, group, feature, filepath, feature_lab){
  dataset$Month = factor(dataset$Month, levels = month.abb)
  dataset$Week = factor(dataset$Week, 
                        levels = c("Mon", "Tue", "Wed",
                                   "Thu", "Fri", "Sat",
                                   "Sun"))
  dataset <- dataset[, c(group, feature, "Month", "Week", "Year")]
  box_fea_month <- ggplot(dataset, 
                          aes(x = Month, y = get(feature))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature_lab, "per Month"), 
         x = "Month",
         y = feature_lab)
  
  box_fea_week <- ggplot(dataset, 
                         aes(x = Week, y  = get(feature))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature_lab, "per Weekday"), 
         x = "Weekday",
         y = feature_lab)  
  
  box_fea_year <- ggplot(dataset, 
                         aes(x = Year, y = get(feature))) + 
    geom_boxplot(varwidth = T, fill = "lightblue") + 
    labs(tittle = "Boxplot", 
         subtitle = paste(feature_lab, "per Year"), 
         x = "Year",
         y = feature_lab) 
  
  ggsave(filename=paste(filepath, feature, "_boxplot.png", sep = ""), 
         plot=grid.arrange(box_fea_week,
                           box_fea_month,
                           box_fea_year),
         width = 10, height = 8, dpi = 100)
  
}

# Summary: Density plot by month, weekdays, year
# @dataset: original time series
# @group: group by certain group
# @feature: the feature we want to check
# @filepath: path to save the image
# @feature_lab: x-label of the plots.
# Return: pictures saved in specified filepath
GetDensityplot = function(dataset, group, feature, filepath, feature_lab){
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
         subtitle = paste(feature_lab, "by Month"), 
         x = feature_lab,
         y = "Density")
  
  density_week = ggplot(dataset, 
                        aes(x = get(feature), col = get(group))) + 
    facet_wrap(~ Week) +
    geom_density() + 
    theme(legend.position="none") +
    labs(tittle = "Density plot", 
         subtitle = paste(feature_lab, "by Week"), 
         x = feature_lab,
         y = "Density")
  
  density_year = ggplot(dataset, 
                        aes(x = get(feature), col = get(group))) + 
    facet_wrap(~ Year) +
    geom_density() + 
    theme(legend.position="none") +
    labs(tittle = "Density plot", 
         subtitle = paste(feature_lab, "by Year"), 
         x = feature_lab,
         y = "Density")
  
  
  ggsave(filename=paste(filepath, feature, "_densityplot.png", sep = ""), 
         plot=grid.arrange(density_week,
                           density_month,
                           density_year),
         width = 10, height = 8, dpi = 100)
  
}

# Summary: Moving average plot 
# @dataset: original time series
# @group: group by certain group
# @feature: the feature we want to check
# @filepath: path to save the image
# @ma_order a scale
# Return: pictures saved in specified filepath
GetMAplot = function(dataset, feature, group, ma_order, filepath, feature_lab){
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
         subtitle = paste(feature_lab, "average per", ma_order, "days"), 
         x = "Time",
         y = feature_lab) 
  ggsave(filename=paste(filepath, feature, "_maplot.png", sep = ""), 
         plot=grid.arrange(ma_gpu),
         width = 10, height = 8, dpi = 100)
  
}

# Summary: Change data to wide
# @dataset: dataframe containing time series to be clustered.
# @features_name: the vector of feature names to be considered.
# @key: name of column containing the new column names.
# @obs: name of the row containing the new row names.
# @features_name: name of column containing values.
# Return: the dataframe of the expanded data
GetClusterPrep <- function(dataset, features_name, obs, key) {
  dataset_res <- unique(dataset[,obs])
  for (i in features_name) {
    tmp <- reshape2::dcast(dataset, get(obs) ~ get(key), value.var = i, fill = NaN)
    dataset_res <- cbind(dataset_res, tmp[,-1])
  }
  return(dataset_res)
}



# Modeling---------------------------

# Summary: Bechmark model using median of the training set.
# @tuning_set: dataframe of time series, must include the feature gcu_seconds and memory_gib_seconds.
# @forcast_time: the length of next prediction interval.
# @predict_set: certain predictors to be added in the model (not for ARIMA model, just be consistent with VAR model).
# @y_feature: certain response variables to be considered.
# Return: Prediction of the correponding features.
MedianModel <- function(tuning_set,  
                        forcast_time = 30, predict_set = NULL,
                        y_feature = NULL){
  pred_ls <- list()
  pred_tmp <- NULL
  for (i in 1:length(y_feature)) {
    fit <- median(tuning_set[, y_feature[i]])
    pred_fit <- rep(fit, forcast_time)
    pred_tmp <- cbind(pred_tmp, pred_fit)
  }
  colnames(pred_tmp) <- y_feature
  pred_ls[["prediction"]] <- as.data.frame(pred_tmp)
  
  return(pred_ls)
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
  pred_ls <- list()
  pred_tmp <- NULL
  for (i in 1:length(y_feature)) {
    fit <- arima(tuning_set[, y_feature[i]], order=c(p, 0, q))
    pred_fit <- predict(fit, n.ahead = forcast_time)$pred
    pred_tmp <- cbind(pred_tmp, pred_fit)
  }
  colnames(pred_tmp) <- y_feature
  pred_ls[["prediction"]] <- as.data.frame(pred_tmp)
  return(pred_ls)
}

# Sumamry: get the prediction from lightgbm model.
# @tuning_set: dataframe of time series, must include the feature gcu_seconds and memory_gib_seconds.
# @forcast_time: the length of next prediction interval.
# @predict_set: certain predictors to be added in the model (not for ARIMA model, just be consistent with VAR model).
# @y_feature: certain response variables to be considered.
# @x_features: certain explainatory variables to be considered.
# @features_cat: categorical variables in x_features.
# Return: Prediction of the correponding features.
LgbmModel <- function(tuning_set,  
                      forcast_time = 30, 
                      y_feature = features, 
                      x_features = x_features,
                      features_cat = features_cat,
                      predict_set = validation_set
) {
  
  train_data <- Matrix(as.matrix(tuning_set[, x_features]), sparse=TRUE)
  test_data  <- Matrix(as.matrix(predict_set[, x_features]), sparse=TRUE)
  pred_ls <- list()
  pred_tmp <- NULL
  for (i in y_feature) {
    train_lgbm = lgb.Dataset(data=train_data, label=tuning_set[, i])
    
    para_grid <- list(objective = "regression",
                      metric = "l2",
                      bagging_fraction = 0.8,
                      bagging_freq = 10,
                      feature_fraction = 0.8,
                      min_data_in_bin = 20,
                      min_gain_to_split = 0.001,
                      is_unbalance = TRUE)
    
    model_cv <- lgb.cv(params = para_grid, 
                       data = train_lgbm, 
                       learning_rate = 0.001, 
                       num_leaves = 10,
                       num_threads = 4, 
                       nrounds = 5000, 
                       early_stopping_rounds = 50,
                       eval = "l2",
                       categorical_feature = features_cat, 
                       nfold = 20, 
                       stratified = T,
                       seed = 123)
    
    lgb_fit <- lgb.train(params = para_grid, 
                         data = train_lgbm, 
                         learning_rate = 0.001,
                         num_leaves = 10, 
                         num_threads = 4, 
                         nrounds = model_cv$best_iter,
                         eval_freq = 20, 
                         eval = "l2",
                         categorical_feature = features_cat)
    
    prediction_percell <- predict(lgb_fit, test_data)
    pred_tmp <- cbind(pred_tmp, prediction_percell)
  }
  
  
  colnames(pred_tmp) <- y_feature
  pred_ls[["prediction"]] <- as.data.frame(pred_tmp)
  
  return(pred_ls)
}

## VAR model

# Sumamry: split dataset into training and test set
# @dataset: dataframe
# @interval: the length of the test set
# @hold_day: the number of hold out day to be split
# Return: list of training and test set split
SplitData <- function(dataset, interval = 10, hold_day = 30)  {
  split_ls <- list()
  for (i in 1:(hold_day - interval + 1)) {
    cutoff <- (nrow(dataset) - interval + 1 - i)
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
    X_expand[[i]] <- t(sapply(1:(nrow(dataset)-lag_num), function(x) dataset[x:(x+lag_num-1), features[i]]))
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


# Sumamry: Get the next several predictions only with time series themselves except other predictors.
# @interval: predict next interval.
# @fit: fitted models.
# @X: input predictor matrix (similar dimensions to train_set).
# @features: names of predictors.
# @lag_num: the number of lags to be selected.
# @is.together controls whether use multivariate gaussian family.
# Return: a dataframe of predictions.
PredictVarHelper <- function(interval, fit, X, 
                             features, lag_num = 15,
                             is.together = T) {
  pred_test <- matrix(0, nrow = interval, ncol = length(features))
  X_tmp <- as.matrix(X)
  
  newx_mat <- matrix(0, ncol = ncol(X)*lag_num, nrow = interval+1)
  newx_mat[1,] <- c(as.matrix(X))
  
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
  colnames(pred_test) <- features
  return(pred_test)
}

# Sumamry: Get the next several predictions both with time series themselves and other predictors.
# @interval: predict next interval.
# @fit: fitted models.
# @X: input predictor matrix (similar dimensions to train_set).
# @features: names of predictors.
# @lag_num: the number of lags to be selected.
# @is.together controls whether use multivariate gaussian family.
# @current_add: whether we add current features in the data.
# @test_set: if current_add is true, then we need specify the test set, or default is null.
# Return: a dataframe of predictions.
PredictVarPredictorHelper <- function(interval, fit, 
                                      X, features, lag_num = 15, 
                                      current_add = F, test_set = NULL, 
                                      is.together = T) {
  pred_test <- matrix(0, nrow = interval, ncol = length(features))
  X_tmp <- as.matrix(X)
  newx_mat <- matrix(0, ncol = ncol(X)*lag_num, nrow = interval+1)
  newx_mat[1,] <- c(as.matrix(X))
  x_feature <- colnames(X)
  x_feature_only <- setdiff(x_feature, features)
  
  if (current_add) {
    newx_mat <- cbind(newx_mat, 
                      matrix(0, nrow = nrow(newx_mat), ncol = length(x_feature_only)))
    newx_mat[1, (ncol(newx_mat) - length(x_feature_only) + 1):ncol(newx_mat)] <- 
      as.matrix(test_set[1, x_feature_only])
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
  
  colnames(pred_test) <- features
  return(pred_test)
}
# Sumamry: Get the next several predictions.
# @interval: predict next interval.
# @fit: fitted models.
# @X: input predictor matrix (similar dimensions to train_set).
# @features: names of predictors.
# @lag_num: the number of lags to be selected.
# @is.together controls whether use multivariate gaussian family.
# @current_add: whether we add current features in the data.
# @test_set: if current_add is true, then we need specify the test set, or default is null.
# Return: a dataframe of predictions.
PredictVar <- function(interval, fit, X, 
                       features, lag_num = 15, 
                       current_add = F, test_set = NULL, 
                       x_pred = F, is.together = T) {

  if (x_pred){
    pred_test <- PredictVarHelper(interval, fit, X, 
                                 features, lag_num = lag_num,
                                 is.together = is.together)
  } else {
    pred_test <- PredictVarPredictorHelper(interval, fit, X, 
                                           features, lag_num = lag_num,
                                           is.together = is.together)
  }
  return(list("pred" = pred_test))#, "error" = error))
}

# Sumamry: Fit the VAR model
# @train_set: the dataframe to train the model
# @lag_num: number of lags to be considered
# @lambda: tuning parameter, either a vector or a scalar 
# @y_feature: the name of response time series only
# @x_feature: the name of time series in the predictor
# @weights: given weights by use equal weighted or weighted MSE to fit the model
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
    fit <- glmnet(X, Y, weights = weights, lambda = lambda, family = family, standardize = F)
    
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
      fit <- glmnet(X, Y[,i], weights = weights, lambda = lambda_sel, family = family, standardize = F)
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
    fit <- fit_ls
  }
  
  
  return(list("pred" = pred, "fit" = fit))
}


# Sumamry: Help function for VarModel.
# @data_norm: the dataframe to train the model.
# @lag_num: number of lags to be considered.
# @tune_type: whether use moving forward or expanding forward to tune the parameter.
# @lambda: tuning parameter, either a vector or a scalar .
# @y_feature: the name of response time series only.
# @x_feature: the name of time series in the predictor.
# @weight_type: whether use equal weighted or weighted MSE to fit the model.
# @is.together controls whether use multivariate gaussian family.
# @interval: the prediction interval for forward corss-validation.
# @current_add: whether we add current features in the data.
# @test_set: if current_add is true, then we need specify the test set, or default is null.
# @x_pred: specify whether predict all features.
# Return: the optimal tuning parameter.
VarModelForwardTune <- function(data_norm, 
                                y_feature, 
                                x_feature,
                                tuning_interval, 
                                hold_day, 
                                is.together, 
                                tune_type, 
                                lag_num, 
                                lambda,
                                weight_type,
                                x_pred = x_pred,
                                current_add, 
                                test_set,
                                y_feature_org) {
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
    }
    
    if (weight_type == "weighted_uniform") {
      weights <- (1 : (nrow(train_set)-lag_num)) / (nrow(train_set)-lag_num)
    }
    
    if (weight_type == "weighted_exponential") {
      weights <- exp((1 : (nrow(train_set)-lag_num))/ (nrow(train_set)-lag_num)) 
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
          mse[[k]][i,j] <- mean((test_set[,y_feature_org[k]] - res$pred[,y_feature_org[k]])^2)
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
  return(lambda_min)
}


# Sumamry: Help function for VarModelUniformTune.
# @train_set: the dataframe to train the model.
# @lag_num: number of lags to be considered.
# @lambda: tuning parameter, either a vector or a scalar .
# @y_feature: the name of response time series only.
# @x_feature: the name of time series in the predictor.
# @weights: given weights by use equal weighted or weighted MSE to fit the model.
# @is.together controls whether use multivariate gaussian family.
# @current_add: whether we add current features in the data.
# @test_set: if current_add is true, then we need specify the test set, or default is null.
# @x_pred: specify whether predict all features.
# Return: the optimal tuning parameter.
VarFitUniform <- function(train_set, lag_num = 15, lambda, 
                          y_feature, x_feature, weights = NULL, 
                          is.together = T, x_pred = T,
                          current_add = F, test_ind = NULL) {
  
  n <- nrow(train_set)
  Y <- train_set[(lag_num+1):n, y_feature]
  X <- train_set[, x_feature]
  
  # Re-construct data
  X_expand <- ExpandData(x_feature, X, lag_num)
  
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
    pred_mat <- NULL
    for (i in 1:length(test_ind)) {
      test_ind_i <- test_ind[i]
      X_new <- train_set[(test_ind_i - lag_num + 1 - i):(test_ind_i - i), x_feature]
      
      pred_test <- PredictVar(interval = 1, 
                              fit = fit, 
                              X = X_new, 
                              features = y_feature, 
                              lag_num = lag_num, 
                              current_add = current_add, 
                              test_set = NULL, 
                              x_pred = x_pred,
                              is.together = is.together)
      pred_mat <- rbind(pred_mat, pred_test$pred)
    }
    
  }else{
    family = "gaussian"
    error <- NULL
    fit_ls <- list()
    
    for (i in 1:length(y_feature)) {
      if (is.list(lambda)) {
        lambda_sel <- lambda[[i]]
      } else {
        lambda_sel <- lambda
      }
      fit <- glmnet(X, c(Y[,i]), weights = weights, lambda = lambda_sel, family = family)
      fit_ls[[i]] <- fit
    }
    
    # Predict
    pred_mat <- NULL
    for (i in 1:length(test_ind)) {
      test_ind_i <- test_ind[i]
      X_new <- train_set[(test_ind_i - lag_num + 1 - i):(test_ind_i - i), x_feature]
      
      pred_test <- PredictVar(interval = 1, 
                              fit = fit_ls, 
                              X = X_new, 
                              features = y_feature, 
                              lag_num = lag_num, 
                              current_add = current_add, 
                              test_set = NULL, 
                              x_pred = x_pred, 
                              is.together = is.together)
      pred_mat <- rbind(pred_mat, pred_test$pred)
    }
    
  }
  
  
  return(list("pred" = pred_mat))
}


# Sumamry: Help function for VarModel. Tune the parameter by uniformly selecting test data.
# @data_norm: the dataframe to train the model.
# @lag_num: number of lags to be considered.
# @tune_type: just to be consistent with VarModelForwardTune.
# @lambda: tuning parameter, either a vector or a scalar .
# @y_feature: the name of response time series for model fitting.
# @x_feature: the name of time series in the predictor.
# @y_feature_org: the name of response time series for output only.
# @weight_type: whether use equal weighted or weighted MSE to fit the model.
# @is.together controls whether use multivariate gaussian family.
# @interval: the prediction interval for forward corss-validation.
# @current_add: whether we add current features in the data.
# @test_set: if current_add is true, then we need specify the test set, or default is null.
# @x_pred: specify whether predict all features.
# Return: the optimal tuning parameter.
VarModelUniformTune <- function(data_norm, 
                                y_feature, 
                                x_feature, 
                                hold_day, 
                                is.together, 
                                tune_type, 
                                lag_num, 
                                lambda,
                                weight_type,
                                x_pred = x_pred,
                                current_add, 
                                test_set = NULL,
                                y_feature_org) {
  # Parameter tuning
  if (is.together) {
    mse = rep(0, length(lambda))
  } else {
    mse = lapply(1:length(y_feature), function(x) rep(0, length(lambda)))
  }
  
  # Split data
  n = nrow(data_norm)
  test_ind <- floor(seq.int(1+lag_num, n, by = lag_num)) 
  
  train_set <- data_norm[-test_ind,]
  test_set <- data_norm[test_ind,]
  
  if (weight_type == "equal") {
    weights <- rep(1, nrow(train_set)-lag_num)
  }
  
  if (weight_type == "weighted_uniform") {
    weights <- (1 : (nrow(train_set)-lag_num)) / (nrow(train_set)-lag_num)
  }
  
  if (weight_type == "weighted_exponential") {
    weights <- exp((1 : (nrow(train_set)-lag_num))/ (nrow(train_set)-lag_num)) 
  }
  
  for (j in 1:length(lambda)) {
    res <- VarFitUniform(train_set = train_set, 
                         lag_num = lag_num, 
                         lambda = lambda[j], 
                         y_feature = y_feature, 
                         x_feature = x_feature, 
                         weights = weights, 
                         is.together = is.together, 
                         x_pred = x_pred,
                         current_add = current_add, 
                         test_ind = test_ind)
    if (is.together) {
      mse[j] <- mean(unlist((test_set[,y_feature_org] - res$pred[,y_feature_org])^2))
    } else {
      for (k in 1:length(mse)) {
        mse[[k]][j] <- mean((test_set[,y_feature_org[k]] - res$pred[,y_feature_org[k]])^2)
      }
    }
  }
  
  
  if (is.together) {
    mse_cv <- mse
    plot(mse_cv, type = "l", col = 2)
    lambda_min <- lambda[which.min(mse_cv)]
  } else {
    mse_cv <- lapply(mse, function(x) {
      tmp <- x
      plot(tmp, type = "l", col = 2)
      return(tmp)
    })
    lambda_min <- lapply(mse_cv, function(x) lambda[which.min(x)])
  }
  return(lambda_min)
}


# Sumamry: Select tuning parameters and get predictions.
# @dataset: the dataframe to train the model.
# @tuning_interval: the length of interval of test set in parameter tuning procedure.
# @lambda: tuning parameter, either a vector or a scalar .
# @hold_day: the number of hold out day to tune the model.
# @lag_num: number of lags to be considered.
# @x_feature: the name of time series in the predictor.
# @y_feature: the name of response time series only.
# @is.together controls whether use multivariate gaussian family.
# @weight_type: whether use equal weighted or weighted MSE to fit the model.
# @forcast_time: the prediction interval for forward corss-validation.
# @predict_set: the test set to tune the parameter.
# @x_pred: specify whether predict all features.
# @current_add: whether we add current features in the data.
# @tuning_parameter: specify the tuning parameter.
# Return: Predictions for next intervals and the tuning parameter.
VarModel <- function(dataset, tuning_interval = 10, lambda = seq(0.0001,0.005, length.out = 20), 
                     hold_day = 30, lag_num = 15, x_feature = x_feature, y_feature = y_feature,
                     is.together = T, weight_type = "equal", forcast_time = forcast_time,
                     predict_set = validation_set_new, x_pred = T, current_add = F, 
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
    if (tune_type %in% c("moving", "expand")) {
      lambda_min <- VarModelForwardTune(data_norm, 
                                        y_feature, 
                                        x_feature,
                                        tuning_interval, 
                                        hold_day, 
                                        is.together, 
                                        tune_type, 
                                        lag_num, 
                                        lambda,
                                        weight_type,
                                        x_pred = x_pred,
                                        current_add, 
                                        test_set,
                                        y_feature_org)
    }
    if (tune_type == "uniform") {
      lambda_min <- VarModelUniformTune(data_norm, 
                                        y_feature, 
                                        x_feature, 
                                        hold_day, 
                                        is.together, 
                                        tune_type, 
                                        lag_num, 
                                        lambda,
                                        weight_type,
                                        x_pred = x_pred,
                                        current_add, 
                                        test_set = NULL,
                                        y_feature_org)
    }
  } else {
    lambda_min <- tuning_parameter
  }
  
  
  # Predict
  if (weight_type == "equal") {
    weights <- rep(1, nrow(data_norm)-lag_num)
  }
  
  if (weight_type == "weighted_uniform") {
    weights <- (1 : (nrow(data_norm)-lag_num)) / (nrow(data_norm)-lag_num)
  }
  
  if (weight_type == "weighted_exponential") {
    weights <- exp((1 : (nrow(data_norm)-lag_num))/ (nrow(data_norm)-lag_num)) 
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
  
  pred_ls <- list()
  pred_ls[["prediction"]] <- as.data.frame(pred_normback)
  pred_ls[["tuning_parameter"]] <- lambda_min
  pred_ls[["fit"]] <- pred$fit
  pred_ls[["normal_para"]] <- list("mean" = data_mean, "sd" = data_sd)
  return(pred_ls)
}


# Validate---------------------------

# Sumamry: Define the transformation method
# @dataset: dataframe.
# @initial_data: contains the initial values before the transformation.
# @features: name of the features we want to transform.
# @tranform_type: type of transformation.
# @season: the seasonality want to be added back.
# @missingdata: dataframe include the index of missing data and its original values.
# Return: transform back the transformed data
Backtransform <- function(dataset, 
                          initial_data = initial_data,
                          features = c("gcu_seconds", "memory_gib_seconds"),
                          tranform_type = "log_diff", season = NULL,
                          missingdata = NULL) {
  
  dataset$date <- as.Date(dataset$date)
  if (!is.null(season)) {
    for (j in features) {
      for (i in (season+1):nrow(dataset)) {
        dataset[i,j] <- dataset[i-season, j] + dataset[i, j] 
      }
    }
  }
  
  if (!is.null(missingdata)) {
    missingdata$date <- as.Date(missingdata$date)
    missingdata <- missingdata[as.Date(missingdata$date) %in% as.Date(dataset$date),]
    for (i in features) {
      miss_ind <- which(as.Date(dataset$date) %in% as.Date(missingdata$date[missingdata$features == i]))
      dataset[miss_ind, i] <- missingdata$original_data[missingdata$features == i]
    }
  }
  
  if (tranform_type == "log_diff") {
    time_diff <- dataset[1, "date"] - as.Date(initial_data$date)
    
    data_org <- as.data.frame(matrix(0, ncol = length(features), nrow = (nrow(dataset)+1)))
    colnames(data_org) <- features
    data_org[1,] <- log(initial_data[,features])
    
    for (i in 1:nrow(dataset)) {
      data_org[i+1,] <- dataset[i,features] + data_org[i,]
    }
    res <- exp(data_org)
    res <- res[-1,]
    res$date <- initial_data$date + time_diff*(1:nrow(res))
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
# @season: the seasonality want to be added back.
# @missingdata: dataframe include the index of missing data and its original values.
# Return the results for gcu and memeory
ValidateModel <- function(validate_type = "expand",
                          model = ArimaModel, 
                          tuning_set = tuning_set, 
                          validation_set = validation_set, 
                          validation_interval = 14, 
                          metric = "MSE", 
                          is.plot = TRUE, 
                          is.origin = TRUE, 
                          transform.fun = Backtransform, 
                          initial_data = initial_data,
                          is.tuning = T,
                          season = 7,
                          missingdata = missingdata,
                          features = c("gcu_seconds", "memory_gib_seconds"),
                          ...
){
  
  first_season <- tuning_set[1:season,]
  data_split <- SplitData(rbind(tuning_set[(season+1):nrow(tuning_set),], validation_set), 
                          interval = validation_interval, 
                          hold_day = nrow(validation_set))
  validation_times <- length(data_split)
  
  res <- list()
  error <- lapply(1:length(features), function(x) NULL)
  error <- setNames(error, features)
  
  # If need to transform back to the original data, transform back.
  if (is.origin) {
    data_origin_all <- transform.fun(rbind(tuning_set, validation_set),
                                     initial_data = initial_data,
                                     features = features,
                                     tranform_type = "log_diff",
                                     season = season,
                                     missingdata = missingdata)
    data_origin_pred_ls <- list()
  }
  
  # Validate model
  tuning_parameter <- NULL
  
  
  
  for (i in 1:validation_times) {
    if (validate_type == "expand") {
      tuning_set_start = 1
    }
    if (validate_type == "moving") {
      tuning_set_start = 1 + (i - 1)
    }
    tune_set_new <- data_split[[i]]$train_set[tuning_set_start:nrow(data_split[[i]]$train_set), ]
    validation_set_new <- data_split[[i]]$test_set
    
    if (is.null(tuning_parameter)) {
      res[[i]] <- model(tune_set_new,
                        forcast_time = validation_interval,
                        predict_set = validation_set_new,
                        y_feature = features,
                        ...)
      
      if (is.tuning) {
        tuning_parameter <- res[[i]]$tuning_parameter
      }
    } else {
      res[[i]] <- model(tune_set_new,
                        forcast_time = validation_interval,
                        predict_set = validation_set_new,
                        y_feature = features,
                        tuning_parameter = tuning_parameter,
                        ...)
    }
    
    time_diff <- tune_set_new[2, "date"] - tune_set_new[1, "date"]
    if (is.origin) {
      validation_set_pred <- as.data.frame(rbind(first_season[,features],
                                                 as.matrix(data_split[[i]]$train_set[,features]), 
                                                 res[[i]]$prediction))
      colnames(validation_set_pred) <- features
      date_lab <- data_split[[i]]$train_set[, "date"]
      date_lab <- c(first_season$date,
                    date_lab, 
                    date_lab[length(date_lab)] + time_diff * (1:validation_interval))
      validation_set_pred$date <- date_lab
      data_origin_pred <- transform.fun(validation_set_pred,
                                        initial_data = initial_data,
                                        features = features,
                                        tranform_type = "log_diff",
                                        season = season,
                                        missingdata = missingdata)
      
      data_origin_pred <- data_origin_pred[(nrow(validation_set_pred) - validation_interval + 1): nrow(data_origin_pred), ]
      validation_set_new_org <- data_origin_all[data_origin_all$date > tune_set_new[nrow(tune_set_new), "date"],][1:validation_interval,]
      
      for (j in features) {
        error[[j]][[i]] <- data_origin_pred[, j] - 
          validation_set_new_org[, j]
      }
      data_origin_pred_ls[[i]] <- data_origin_pred
      
    } else {
      
      for (j in features) {
        error[[j]][[i]] <- res[[i]]$prediction[, j] - 
          validation_set_new[, j]
      }
      
    }
  }
  
  eval_res_mse <- lapply(features, function(x) mean(unlist(error[[x]])^2))
  eval_res_mse <- setNames(eval_res_mse, features)
  eval_res_mae <- lapply(features, function(x) median(abs(unlist(error[[x]]))))
  eval_res_mae <- setNames(eval_res_mae, features)
  
  
  pred_transform <- lapply(res[seq(1, length(data_split), validation_interval)], function(x) x$prediction)
  pred_transform <- as.data.frame(do.call("rbind", pred_transform))
  pred_data_transform <- pred_transform
  
  if (is.origin) {
    par(mfrow=c(2,2))
    pred_data_org <- do.call("rbind", data_origin_pred_ls[seq(1, length(data_split), validation_interval)])
    plot_data <- rbind(data_origin_all[1:nrow(tuning_set), c(features, "date")], pred_data_org)
    pred_transform <- rbind(tuning_set[, features], pred_transform[, features])
    colnames(pred_transform) <- features
  }else{
    pred_data_org <- NULL
  }
  
  if (is.plot == TRUE) {
    ValidatePlots(tuning_set, validation_set, features, plot_data, data_origin_all, pred_transform, is.origin,
                  main = paste(as.character(substitute(model)), "for", validation_interval, "days prediction")) 
  }
  return(
    list("Metric" = list("mae" = eval_res_mae, "mse" = eval_res_mse), 
         "prediction_org" = pred_data_org, 
         "prediction_trans" = pred_data_transform, 
         "model" = res)
  )
  
}

# Sumamry: Get the plots for validation procedure.
# @tuning_set: the dataframe of the tuning set
# @validation_set: data to do model selection
# @features: name of the features we want to transform.
# @is.origin: whether show the results transformed back on the origin data
# @plot_data: the back transformation function.
# @data_origin_all: the original data before transfermation.
# @pred_transform: the seasonality want to be added back.
# @main: the caption of the plots.
# Return the plots.
ValidatePlots <- function(tuning_set, validation_set, features, plot_data, data_origin_all, pred_transform, is.origin,
                          main) {
  data_transform_all <- rbind(tuning_set, validation_set)
  xlabel <- data_transform_all$date
  
  if (is.origin) {
    par(mfrow=c(2,2))
    for (j in features) {
      # plot origin
      # red line is the model prediction
      plot(xlabel[1:nrow(plot_data)], plot_data[, j], type = "l", col = 2, ylab = j, xlab = "Time", main = "Original") 
      lines(xlabel,data_origin_all[, j], col = 1)
      abline(v = xlabel[nrow(tuning_set)], col = 3, lty = 2)
      
      # plot transformed data
      plot(xlabel[1:nrow(pred_transform)], pred_transform[, j], type = "l", col = 2, ylab = j, xlab = "Time", main = "Transformed") 
      lines(xlabel,data_transform_all[, j], col = 1)
      abline(v = xlabel[nrow(tuning_set)], col = 3, lty = 2)
      
    }
    
  }else {
    par(mfrow=c(2,1))
    # plot transformed data
    for (j in features) {
      # plot transformed data
      plot(xlabel[1:nrow(pred_transform)], pred_transform[, j], type = "l", col = 2, ylab = j, xlab = "Time", main = "Transformed") 
      lines(xlabel,data_transform_all[, j], col = 1)
      abline(v = xlabel[nrow(tuning_set)], col = 3, lty = 2)
    }
  }
  title(main, line = -1, outer = TRUE)
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
  pred_tmp <- NULL
  for (i in 1:length(y_feature)) {
    ### Run the bsts model
    y <- tuning_set[,y_feature[i]]
    fit <- bsts::AddLocalLinearTrend(list(), y)
    fit <- bsts::AddSeasonal(fit, y, nseasons = 30)
    
    bsts.model <- bsts::bsts(y,
                             state.specification = fit,
                             niter = 500) 
    
    burn <- SuggestBurn(0.1, bsts.model)
    pred <- predict(bsts.model,
                    h = forcast_time,
                    burn = burn)
    pred_tmp <- cbind(pred_tmp, pred$mean)
  }
  colnames(pred_tmp) <- y_feature
  pred_ls[["prediction"]] <- as.data.frame(pred_tmp)
   
  return(pred_ls)
}


# Result Table---------------------------
# Summary tables for model comparison
# @model: model want to call
# @tuning_set: the dataframe of the tuning set
# @is.tuned: whether model need to tune the parameters
# Return: MSE for all features, and plots for the fitted value
GetModelValTable <- function(model, is.tuning, ...) {
  validation_interval <- c(3, 7, 14)
  mse_var <- matrix(0, nrow = 2, ncol = length(validation_interval))
  mae_var <- matrix(0, nrow = 2, ncol = length(validation_interval))
  
  cutoff <- 7
  season <- 7
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
                                      features = c("gcu_seconds", "memory_gib_seconds"),
                                      ...
                                      
    )
    mse_var[1,i] <- var_res_together$Metric$mse$gcu_seconds
    mse_var[2,i] <- var_res_together$Metric$mse$memory_gib_seconds
    mae_var[1,i] <- var_res_together$Metric$mae$gcu_seconds
    mae_var[2,i] <- var_res_together$Metric$mae$memory_gib_seconds
  }
  return(list("mse" = mse_var, "mae" = mae_var))
}

# Summary tables for different VAR settings
# VAR model accross different prediction time with different tuning interval
# @tuning_set: the dataframe of the tuning set
# @lag_num: number of lags to be considered
# @is.together controls whether use multivariate gaussian family
# @weight_type: whether use equal weighted or weighted MSE to fit the model
# Return: MSE for all features, and plots for the fitted value
GetTunValTable <- function(tuning_set, is.together, lag_num = 28, weight_type = "equal", lambda = lambda) {
  validation_interval <- c(3, 7, 14)
  hold_day <- 21
  mse_var <- matrix(0, nrow = length(hold_day)*2, ncol = length(validation_interval))
  mae_var <- matrix(0, nrow = length(hold_day)*2, ncol = length(validation_interval))
  
  
  initial_data <- data_agg_sum[as.Date(data_agg_sum$date) == (as.Date(tuning_set[1,]$date)-1), ]
  
  ### missing data need to be changed
  
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
                                        lambda = lambda, 
                                        hold_day = hold_day_j, 
                                        lag_num = lag_num, 
                                        x_feature = c("gcu_seconds", "memory_gib_seconds"), 
                                        is.together = is.together, 
                                        weight_type = weight_type,
                                        x_pred = T, 
                                        current_add = F,
                                        is.tuning = T,
                                        season = 7,
                                        missingdata = missingdata,
                                        features = c("gcu_seconds", "memory_gib_seconds")
      )
      mse_var[2*j-1,i] <- var_res_together$Metric$mse$gcu_seconds
      mse_var[2*j,i] <- var_res_together$Metric$mse$memory_gib_seconds
      mae_var[2*j-1,i] <- var_res_together$Metric$mae$gcu_seconds
      mae_var[2*j,i] <- var_res_together$Metric$mae$memory_gib_seconds
      
    }
  }
  return(list("mse" = mse_var, "mae" = mae_var))
  
}

# Summary: Zoom in the plots for fitted values vesus original values
# @dataset: dataframe containing the prediction for original data
# @actual_set: contains the original values before the transformation
# @interval: interval of the zoom in part
# @breakline: add the vertical line indicating the start date of the prediction
# @features_name: name of the features to be ploted
# Return: pictures of zoomed in plots
GetZoomIn <- function(dataset, actual_set, interval, breakline, features_name) {
  par(mfrow=c(2,1))
  for (i in features_name) {
    plot(dataset$date[interval], dataset[interval, i], type = "l", 
         col = 2, ylab = i, xlab = "Time", main = "Original")
    lines(actual_set$date[interval], 
          actual_set[interval, i], col = 1)
    abline(v = breakline, col = 3, lty = 2)
  }
  
}

# Summary: Find the estimators for the given model.
# @model_fit: contains information about fitted object and normalization parameters, etc. It depends on the previous fitted model.
# @dataset: dataframe for prediction.
# @type: type of model used.
# @lag_num: number of lags.
# @interval: the length of prediction time.
# @features: name of the output features.
# Return: the list of estimators.
GetEstimator <- function(model_fit, dataset, type = "var", lag_num = NULL, interval = 14, features,
                         ...) {
  if (type == "var") {
    estimator_ls <- list()
    for (i in features) {
      dataset[,i] <- (dataset[,i] - model_fit$normal_para$mean[i])
      dataset[,i] <- dataset[,i] / model_fit$normal_para$sd[i]
    }
    
    for (i in 1:(nrow(dataset) - interval - lag_num + 1)) {
      newdata <- dataset[i:(lag_num + i - 1), ]
      estimator <- PredictVar(interval = interval, model_fit$fit, newdata[, features], 
                              features = features, lag_num = lag_num, 
                              ...)  
      
      estimator_ls[[i]] <- estimator$pred  * 
        matrix(rep(model_fit$normal_para$sd, interval), ncol = length(features), byrow = T) + 
        matrix(rep(model_fit$normal_para$mean, interval), ncol = length(features), byrow = T)
      
    }
    
  }
  return(estimator_ls)
}

# Summary: Find the confidence interval for the given model.
# @estimator_ls: the list of estimators.
# @data_trans: dataframe for transformed data.
# @initial_data_mat: the original dataset.
# @transform.fun: transformation function.
# @features: name of the output features
# @tranform_type: type of transformation.
# @season: the seasonality want to be added back.
# @missingdata: dataframe include the index of missing data and its original values.
# Return: the list of daily errors, sum of interval errors, and the original estimators.
GetError <- function(data_trans,
                     estimator_ls, 
                     initial_data_mat, 
                     transform.fun = Backtransform, 
                     features, 
                     tranform_type = "log_diff", 
                     season, 
                     missingdata) {
  error_ls_day <- list()
  error_ls_sum <- list()
  estimator_org_ls <- list()
  data_trans$date <- as.Date(data_trans$date)
  initial_data_mat$date <- as.Date(initial_data_mat$date)
  
  for (i in 1:length(estimator_ls)) {
    estimator_tmp <- as.data.frame(estimator_ls[[i]])
    
    estimator_tmp$date <- as.Date(initial_data_mat$date[i] + (season + lag_num + 1 : interval))
    end_idx_i <- which(as.Date(data_trans$date) == (as.Date(estimator_tmp$date[1]) - 1))
    dataset_i <- rbind(data_trans[(end_idx_i - season + 1):end_idx_i, c(features, "date")], estimator_tmp)
    initial_data_i <- initial_data_mat[as.Date(initial_data_mat$date) == (as.Date(dataset_i[1,]$date) - 1), ]
    
    data_origin_i <- Backtransform(dataset_i,
                                   initial_data = initial_data_i,
                                   features = features,
                                   tranform_type = tranform_type, 
                                   season = season,
                                   missingdata = missingdata)
    # Check the difference between original data and predicted data
    plot(data_origin_i$date, data_origin_i$gcu_seconds, type = "l", main = i)
    lines(initial_data_mat$date, initial_data_mat$gcu_seconds, col=2)
    
    estimator_org_i <- data_origin_i[(nrow(data_origin_i) - interval + 1):nrow(data_origin_i),]
    estimator_org_ls[[i]] <- estimator_org_i
    
    org_idx <- as.Date(initial_data_mat$date) %in% as.Date(estimator_org_i$date)
    error_ls_day[[i]] <- initial_data_mat[org_idx, features] - 
      estimator_org_i[,features] 
    
    error_ls_sum[[i]] <- apply(initial_data_mat[org_idx, features], 2, sum) - 
      apply(estimator_org_i[,features], 2, sum) 
    
    
  }
  
  return(list("error_each" = error_ls_day, "error_sum" = error_ls_sum, "estimator_org" = estimator_org_ls))
  
}

# Summary: Find the confidence interval for the given model.
# @model_fit: contains information about fitted object and normalization parameters, etc. It depends on the previous fitted model.
# @dataset: dataframe for prediction.
# @data_trans: dataframe for transformed data.
# @data_org: dataframe for original data.
# @type: type of model used.
# @lag_num: number of lags.
# @interval: the length of prediction time.
# @features: name of the output features.
# @transform.fun: transformation function.
# @CI: the quantile of confidence interval.
# @prediction_org: the prediction of original data.
# Return: the prediction interval for daily usage and summation of the interval usage.
GetConf <- function(model_fit, 
                    dataset, 
                    data_trans,
                    data_org,
                    type = "var", 
                    lag_num = NULL, 
                    interval = 14, 
                    features, 
                    transform.fun = Backtransform,
                    CI = c(0.25, 0.95),
                    prediction_org,
                    ...) {
  initial_start_idx <- which(as.Date(data_org$date) == (as.Date(dataset[1,]$date)-1)) 
  initial_data_mat <- data_org[(initial_start_idx - season):(nrow(dataset) + initial_start_idx), ]
  
  
  estimator_ls <- GetEstimator(model_fit, 
                               dataset = dataset, 
                               type = type, 
                               lag_num = lag_num, 
                               interval = interval, 
                               features = features,
                               ...)
  
  error_ls <- GetError(data_trans,
                       estimator_ls, 
                       initial_data_mat, 
                       transform.fun = transform.fun, 
                       features, 
                       tranform_type = "log_diff", 
                       season, 
                       missingdata)
  
  
  error_quantile <- list()
  pred_interval_org <- list()
  error_sum_quantile <- list()
  pred_interval_sum_org <- list()
  for (i in 1:length(features)) {
    # CI for daily
    pred_i <- lapply(error_ls$error_each, function(x) x[,features[i]] )
    pred_i <- do.call("rbind", pred_i)
    error_quantile[[features[i]]] <- matrix(unlist(lapply(1:ncol(pred_i), function(x) quantile(pred_i[,x], probs = CI))), 
                                            nrow = interval, 
                                            byrow = T)
    pred_interval_org[[i]] <- matrix(0, nrow = interval, ncol = 2)
    lowerbound <- error_quantile[[features[i]]][, 1] + prediction_org[, features[i]]
    lowerbound[lowerbound < 0] <- 0
    pred_interval_org[[i]][, 1] <- lowerbound
    pred_interval_org[[i]][, 2] <- error_quantile[[features[i]]][, 2] + prediction_org[, features[i]]
    
    # CI for sum
    pred_sum_i <- lapply(error_ls$error_sum, function(x) x[features[i]] )
    pred_sum_i <- do.call("rbind", pred_sum_i)
    error_sum_quantile[[features[i]]] <- matrix(unlist(lapply(1:ncol(pred_sum_i), 
                                                              function(x) quantile(pred_sum_i[,x], probs = CI))), 
                                                nrow = 1, 
                                                byrow = T)
    pred_interval_sum_org[[i]] <- matrix(0, nrow = 1, ncol = 2)
    lowerbound <- error_sum_quantile[[features[i]]][, 1] + sum(prediction_org[, features[i]])
    lowerbound[lowerbound < 0] <- 0
    pred_interval_sum_org[[i]][, 1] <- lowerbound
    pred_interval_sum_org[[i]][, 2] <- error_sum_quantile[[features[i]]][, 2] + sum(prediction_org[, features[i]])
    
  }
  return(list("CI_day" = pred_interval_org, "CI_sum" = pred_interval_sum_org))
  
}
