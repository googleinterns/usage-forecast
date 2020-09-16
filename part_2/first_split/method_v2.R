rm(list = ls())
setwd("/Volumes/GoogleDrive/My Drive/alsta_analysis_LA")

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
source("functions_v2.R")

####################################
##### Section 2: Preprocessing #####
####################################

### Impute data ###
# Remove future data.
end_date <- as.POSIXlt("2018-03-28", tz = "America/Los_Angeles")

# Impute range: 2017-08-01 to 2017-09-01.
# Read the original data to get the memory information.
data_agg_sum <- readRDS("data_agg_sum.rds")
data_impute_prep_mem <- data_agg_sum[data_agg_sum$date < end_date,]
data_impute_prep_mem <- data_impute_prep_mem[data_impute_prep_mem$date <= as.POSIXct("2017-09-02", tz = "America/Los_Angeles") & 
                                               data_impute_prep_mem$date >= as.POSIXct("2017-08-01", tz = "America/Los_Angeles"),
                                             ]
data_impute_prep <- data_agg_sum[data_agg_sum$date < end_date,]
data_impute_prep <- data_impute_prep[data_impute_prep$date > as.POSIXct("2017-09-02", tz = "America/Los_Angeles") | 
                                       data_impute_prep$date < as.POSIXct("2017-08-01", tz = "America/Los_Angeles"),]
data_impute_prep <- data_impute_prep[data_impute_prep$Month %in% month.abb[c(5:9)], ]
summary(data_impute_prep)
head(data_impute_prep)

# Impute using Mixed effect model.
fit_impute <- lm(gcu_seconds ~ memory_gib_seconds + Year + Month + Week + Day,
                 data = data_impute_prep) 
summary(fit_impute)
data_impute_gcu <- predict(fit_impute, data_impute_prep_mem)

data_agg_sum[data_agg_sum$date <= as.POSIXct("2017-09-02", tz = "America/Los_Angeles") & 
               data_agg_sum$date >= as.POSIXct("2017-08-01", tz = "America/Los_Angeles"), "gcu_seconds"] <- 
  data_impute_gcu

### Transform data ###
# Transform: take difference of log.
features_name <- c("gcu_seconds", "memory_gib_seconds", "gpu")
breaktime <- as.POSIXct("2018-03-28", tz = "America/Los_Angeles")

data_agg_sum_trans_log <- as.data.frame(sapply(features_name, function(x) {
  log(data_agg_sum[2:dim(data_agg_sum)[1], x]) - log(data_agg_sum[1:(dim(data_agg_sum)[1]-1), x])}))
data_agg_sum_trans_log$date <- data_agg_sum$date[2:dim(data_agg_sum)[1]]
data_agg_sum_trans_log$cell <- "total"

# plots of the data after imputation
GetLineplot(dataset = data_agg_sum_trans_log[data_agg_sum_trans_log$date < breaktime,], "cell", filepath = "plots/transform/impute_") 
GetLineplot(dataset = data_agg_sum_trans_log, "cell", filepath = "plots/transform/impute_all_") 

# Check the boxplots and density plots for transformed data and original data
# Original data
data_agg_org_sum_plt <- GetTimeInfo(data_agg_sum)
data_agg_org_sum_plt <- data_agg_org_sum_plt[data_agg_sum$date <
                                               as.POSIXlt("2018-03-27", tz = "America/Los_Angeles"),]

GetBoxplot(dataset = data_agg_org_sum_plt, 
           group = "cell",
           feature = "gcu_seconds",
           filepath = "./plots/org_sum_gcu_")

GetBoxplot(dataset = data_agg_org_sum_plt, 
           group = "cell",
           feature = "memory_gib_seconds",
           filepath = "./plots/org_sum_mem_")

# Transformed data
data_agg_trans_sum_plt <- GetTimeInfo(data_agg_sum_trans_log)
data_agg_trans_sum_plt <- data_agg_trans_sum_plt[data_agg_trans_sum_plt$date <
                                                   as.POSIXlt("2018-03-27", tz = "America/Los_Angeles"),]

GetBoxplot(dataset = data_agg_trans_sum_plt, 
           group = "cell",
           feature = "gcu_seconds",
           filepath = "./plots/tran_sum_")

GetBoxplot(dataset = data_agg_trans_sum_plt, 
           group = "cell",
           feature = "memory_gib_seconds",
           filepath = "./plots/tran_sum_")

GetDensityplot(data_agg_trans_sum_plt, "cell", "gcu_seconds", "./plots/tran_sum_")
GetDensityplot(data_agg_trans_sum_plt, "cell", "memory_gib_seconds", "./plots/tran_sum_")
plot(data_agg_trans_sum_plt$date, data_agg_trans_sum_plt$memory_gib_seconds, type = "l")
plot(data_agg_trans_sum_plt$date, data_agg_trans_sum_plt$gcu_seconds, type = "l")

### Data cleanning ###
# Remove observations that is unusual higher than others.
data_agg_sum_trans_log <- data_agg_sum_trans_log[data_agg_sum_trans_log$date <
                                                   as.POSIXlt("2018-04-28", tz = "America/Los_Angeles"),]
upper_clean_gcu <- quantile(data_agg_sum_trans_log$gcu_seconds, 0.999)
lower_clean_gcu <- quantile(data_agg_sum_trans_log$gcu_seconds, 0.001)
upper_clean_mem <- quantile(data_agg_sum_trans_log$memory_gib_seconds, 0.995)
lower_clean_mem <- quantile(data_agg_sum_trans_log$memory_gib_seconds, 0.005)

ind_clean_gcu <- which(data_agg_sum_trans_log$gcu_seconds > upper_clean_gcu | 
                         data_agg_sum_trans_log$gcu_seconds < lower_clean_gcu)
ind_clean_mem <- which(data_agg_sum_trans_log$memory_gib_seconds > upper_clean_mem | 
                         data_agg_sum_trans_log$memory_gib_seconds < lower_clean_mem)

# Record the index and date for these unusual observations.
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

# Check the results of imputation.
plot(data_agg_sum_trans_log$date, data_agg_sum_trans_log$gcu_seconds, type = "l")
lines(data_agg_sum_trans_log_prep$date, data_agg_sum_trans_log_prep$gcu_seconds, col = 2)
plot(data_agg_sum_trans_log$date,data_agg_sum_trans_log$memory_gib_seconds, col = 1, type = "l")
lines(data_agg_sum_trans_log_prep$date, data_agg_sum_trans_log_prep$memory_gib_seconds, col = 2)

# Save results
saveRDS(data_agg_sum_trans_log_prep, "data_agg_sum_trans_log_prep_rf.rds")
data_agg_sum_trans_log_prep <- readRDS("data_agg_sum_trans_log_prep_rf.rds")

### De-seasonality ###
# Transformed data de-seasonality
data_agg_sum_trans_log <- data_agg_sum_trans_log_prep[,5:9]
data_agg_trans_sum_plt <- GetTimeInfo(data_agg_sum_trans_log)
stl(data_agg_trans_sum_plt$memory_gib_seconds, s.window="periodic")
decompose(data_agg_trans_sum_plt$memory_gib_seconds, type = c("multiplicative"), filter = NULL)
decompose(data_agg_trans_sum_plt$gcu_seconds, type = c("multiplicative"), filter = NULL)

stl(data_agg_sum$gcu_seconds, s.window="periodic")
decompose(data_agg_sum$gcu_seconds, type = c("multiplicative"), filter = NULL)


data_agg_sum_trans_log2 <- de_seasonal(data_agg_sum_trans_log, 
                                       features = c("gcu_seconds", "memory_gib_seconds"), 
                                       season = 7,
                                       type = "diff")
# # Test Backtransform function
# season <- 7
# initial_data <- data_agg_sum[1,]
# data_agg_sum_trans_log2_origin <- Backtransform(data_agg_sum_trans_log2,
#                                                 initial_data = initial_data,
#                                                 features = c("gcu_seconds", "memory_gib_seconds"),
#                                                 tranform_type = "log_diff", 
#                                                 season = season, 
#                                                 missingdata = missingdata)
# 
# 
# plot(data_agg_sum_trans_log2_origin$gcu_seconds, type = "l")
# lines(data_agg_sum$gcu_seconds[2:(nrow(data_agg_sum_trans_log2_origin)+1)], col=2)


# Check box plot after de-seasonality
# Transformed data
data_agg_trans_sum_plt_desea <- GetTimeInfo(data_agg_sum_trans_log2)
data_agg_trans_sum_plt_desea <- data_agg_trans_sum_plt_desea[data_agg_trans_sum_plt_desea$date <
                                                               as.POSIXlt("2018-03-27", tz = "America/Los_Angeles"),]

data_agg_trans_sum_plt_desea2 <- data_agg_trans_sum_plt_desea[8:nrow(data_agg_trans_sum_plt_desea),]
GetBoxplot(dataset = data_agg_trans_sum_plt_desea2, 
           group = "cell",
           feature = "gcu_seconds",
           filepath = "./plots/transform/desea_tran_sum_")

GetBoxplot(dataset = data_agg_trans_sum_plt_desea2, 
           group = "cell",
           feature = "memory_gib_seconds",
           filepath = "./plots/transform/desea_tran_sum_")

### Change point detection ###
season = 7
strucchange::breakpoints(gcu_seconds~1, data = data_agg_sum_trans_log[(season+1):nrow(data_agg_sum_trans_log),])
strucchange::breakpoints(memory_gib_seconds~1, data = data_agg_sum_trans_log[(season+1):nrow(data_agg_sum_trans_log),])
strucchange::breakpoints(gcu_seconds~memory_gib_seconds, data = data_agg_sum_trans_log[(season+1):nrow(data_agg_sum_trans_log),]) #828 1095 
changepoint::cpt.mean(data_agg_sum_trans_log$gcu_seconds[(season+1):nrow(data_agg_sum_trans_log)], method = "PELT", Q = 10)
changepoint::cpt.mean(data_agg_sum_trans_log$memory_gib_seconds[(season+1):nrow(data_agg_sum_trans_log)], method = "PELT", Q = 10)

### Check Stationarity ###
# Check Stationarity before de-seasoning.
features_name <- c("gcu_seconds", "memory_gib_seconds")
pval <- c()
for (i in features_name) {
  data_before_ind <- is.na(data_agg_sum_trans_log[(season+1):nrow(data_agg_sum_trans_log),i])
  print(i)
  pval[i] <- tseries::adf.test(data_agg_sum_trans_log[(season+1):nrow(data_agg_sum_trans_log),][!data_before_ind,i], alternative = "stationary")$p.value
  acf(data_agg_sum_trans_log[(season+1):nrow(data_agg_sum_trans_log),][!data_before_ind,i])
  pacf(data_agg_sum_trans_log[(season+1):nrow(data_agg_sum_trans_log),][!data_before_ind,i])
}


plot(data_agg_sum_trans_log$date, data_agg_sum_trans_log$gcu_seconds, col = 1, type = "l")
lines(data_agg_sum_trans_log2$date, data_agg_sum_trans_log2$gcu_seconds, col = 2)

# Check Stationarity after de-seasoning.
features_name <- c("gcu_seconds", "memory_gib_seconds")
pval <- c()
for (i in features_name) {
  data_before_ind <- is.na(data_agg_sum_trans_log2[(season+1):nrow(data_agg_sum_trans_log2),i])
  print(i)
  pval[i] <- tseries::adf.test(data_agg_sum_trans_log2[(season+1):nrow(data_agg_sum_trans_log2),][!data_before_ind,i], alternative = "stationary")$p.value
  acf(data_agg_sum_trans_log2[(season+1):nrow(data_agg_sum_trans_log2),][!data_before_ind,i])
  pacf(data_agg_sum_trans_log2[(season+1):nrow(data_agg_sum_trans_log2),][!data_before_ind,i])
}

### Split data ###
# Get two week time after March 28, 2018 as the comparison set
data_log_impute <- data_agg_sum_trans_log2
hold_date <- 14
end_date <- as.Date(as.POSIXlt("2018-03-28", tz = "America/Los_Angeles")) + hold_date
comparison_set <- data_log_impute[as.Date(data_log_impute$date) < end_date & 
                                    as.Date(data_log_impute$date) >= as.Date(as.POSIXlt("2018-03-28", tz = "America/Los_Angeles")),]

# Get one month time before March 28, 2018 as the hold out set to use walk forward validation test model
data_log_impute <- data_agg_sum_trans_log2
hold_date <- 30
end_date <- as.Date(as.POSIXlt("2018-03-28", tz = "America/Los_Angeles")) - hold_date
train_set_all <- data_log_impute[as.Date(data_log_impute$date) < end_date,]
holddout_set <- data_log_impute[as.Date(data_log_impute$date) >= end_date,]

# Get six weeks time before previous end_date as the validation set to use walk forward validation to select models
hold_date <- 42
end_date <- end_date - hold_date
tuning_set <- train_set_all[as.Date(train_set_all$date) < end_date,]
validation_set <- train_set_all[as.Date(train_set_all$date) >= end_date,]

# Get two weeks time before previous end_date as the train set to train models and parameter tuning
hold_date <- 14
end_date <- end_date - hold_date
train_set_tune <- tuning_set[as.Date(tuning_set$date) < end_date,]
test_set_tune <- tuning_set[as.Date(tuning_set$date) >= end_date,]

###############################
##### Section 2: Modeling #####
###############################
### ARIMA ###
# use acf and pacf plots to select the lag number.
features_select_disk_rm <- c("gcu_seconds", "memory_gib_seconds")
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
  acf(tuning_set[365:nrow(tuning_set),i])
  pacf(tuning_set[365:nrow(tuning_set),i])
  dev.off()
}

p = 7
q = 0
forcast_time = 3

### Check ValidateModel ### 
initial_data <- data_agg_sum[as.Date(data_agg_sum$date) == (as.Date(tuning_set[1,]$date)-1), ]
# initial_data_16rm <- data_agg_sum[as.Date(data_agg_sum$date) == (as.Date(tuning_set[365,]$date)-1), ]
# arima_res <- ValidateModel(validate_type = "moving",
#                            model = ArimaModel,
#                            tuning_set = tuning_set,
#                            validation_set = validation_set,
#                            validation_interval = 3,
#                            metric = "MSE",
#                            is.plot = TRUE,
#                            is.origin = T,
#                            transform.fun = Backtransform,
#                            initial_data = initial_data,
#                            p = p,
#                            q = 0,
#                            is.tuning = F, 
#                            season = 7,
#                            missingdata = missingdata
# )
# 
# var_res_together <- ValidateModel(validate_type = "moving",
#                                   model = MedianModel,  
#                                   tuning_set = tuning_set, 
#                                   validation_set = validation_set, 
#                                   validation_interval = 14,
#                                   metric = "MSE", 
#                                   is.plot = TRUE, 
#                                   is.origin = T, 
#                                   transform.fun = Backtransform, 
#                                   initial_data = initial_data,
#                                   is.tuning = F, season = 7
#                                   
# )

### Compare ValidateModel ### 
# Compare VAR models
lag_num <- 20
season = 7
cutoff = 365
initial_data <- data_agg_sum[as.Date(data_agg_sum$date) == (as.Date(tuning_set[1,]$date)-1), ]
res_notogether <- GetTunValTable(tuning_set, is.together = F, lag_num = lag_num + 10, lambda = seq(0.001,0.05,0.0005)) 
res_notogether_16rm <- GetTunValTable(rbind(data_agg_sum_trans_log[(cutoff - season + 1):cutoff,], 
                                            tuning_set[cutoff:nrow(tuning_set),]), 
                                      is.together = F, 
                                      lag_num = lag_num,
                                      lambda = seq(0.00025,0.3,0.0025)) 
res_together <- GetTunValTable(tuning_set, is.together = T, lag_num = lag_num + 10, lambda = seq(0.002,0.05,0.0005)) 
res_together_16rm <- GetTunValTable(rbind(data_agg_sum_trans_log[(cutoff - season + 1):cutoff,], 
                                          tuning_set[cutoff:nrow(tuning_set),]), is.together = T, lag_num = lag_num, 
                                    lambda = seq(0.00025,0.03,0.00025)) 
res_tunval_table <- rbind(res_notogether, res_notogether_16rm, res_together, res_together_16rm)
colnames(res_tunval_table) <- c(3, 7, 14)
rownames(res_tunval_table) <- rep(c("MSE_gcu", "MSE_memory"),4)
saveRDS(res_tunval_table, "tables/res_tunval_table.rds")
res_tunval_table

# uniform weighted
weight_type = "weighted_uniform"

res_notogether <- GetTunValTable(tuning_set, is.together = F, lag_num = lag_num, weight_type, lambda = seq(0.00005,0.005,0.00005)) 
res_notogether_16rm <- GetTunValTable(rbind(data_agg_sum_trans_log[(cutoff - season + 1):cutoff,], 
                                            tuning_set[cutoff:nrow(tuning_set),]), 
                                      is.together = F, 
                                      lag_num = lag_num, 
                                      weight_type,
                                      lambda = seq(0.0025,0.1,0.0025)) 
res_together <- GetTunValTable(tuning_set, is.together = T, lag_num = lag_num, weight_type, lambda = seq(0.000025,0.003,0.000025)) 
res_together_16rm <- GetTunValTable(rbind(data_agg_sum_trans_log[(cutoff - season + 1):cutoff,], 
                                          tuning_set[cutoff:nrow(tuning_set),]), 
                                    is.together = T, 
                                    lag_num = lag_num, 
                                    weight_type,
                                    lambda = seq(0.005,0.08,0.0025)) 
res_tunval_table <- rbind(res_notogether, res_notogether_16rm, res_together, res_together_16rm)
colnames(res_tunval_table) <- c(3, 7, 14)
rownames(res_tunval_table) <- rep(c("MSE_gcu", "MSE_memory"),4)
saveRDS(res_tunval_table, "tables/res_tunval_table_weight.rds")
res_tunval_table

# exponential weighted
weight_type = "weighted_exponential"
res_notogether <- GetTunValTable(tuning_set, is.together = F, lag_num = lag_num, weight_type, lambda = seq(0.00005,0.01,0.00005)) 
res_notogether_16rm <- GetTunValTable(rbind(data_agg_sum_trans_log[(cutoff - season + 1):cutoff,], 
                                            tuning_set[cutoff:nrow(tuning_set),]), 
                                      is.together = F, 
                                      lag_num = lag_num, 
                                      weight_type,
                                      lambda = seq(0.0025,0.1,0.0025)) 
res_together <- GetTunValTable(tuning_set, is.together = T, lag_num = lag_num + 10, weight_type, lambda = seq(0.0025,0.05,0.0025)) 
res_together_16rm <- GetTunValTable(rbind(data_agg_sum_trans_log[(cutoff - season + 1):cutoff,], 
                                          tuning_set[cutoff:nrow(tuning_set),]), 
                                    is.together = T, 
                                    lag_num = lag_num, 
                                    weight_type,
                                    lambda = seq(0.0025,0.08,0.0025)) 
res_tunval_table <- rbind(res_notogether, res_notogether_16rm, res_together, res_together_16rm)
colnames(res_tunval_table) <- c(3, 7, 14)
rownames(res_tunval_table) <- rep(c("MSE_gcu", "MSE_memory"),4)
saveRDS(res_tunval_table, "tables/res_tunval_table_expweight.rds")
res_tunval_table


# Compare different models
models <- c("MedianModel", "ArimaModel", "VAR", "BaysianModel" )
validation_interval <- c(3, 7, 14)
mse_var <- matrix(0, nrow = length(models)*2, ncol = length(validation_interval))

model_median <-  GetModelValTable(model = MedianModel, 
                                  is.tuning = F
)

lag_num = 14
model_arima <-  GetModelValTable(model = ArimaModel, 
                                 is.tuning = F,
                                 p = lag_num,
                                 q = 0
)

x_feature <- c("gcu_seconds", "memory_gib_seconds")
lag_num <- 20
model_var <-  GetModelValTable(model = VarModel,
                               is.tuning = T,
                               tuning_interval = 7, 
                               lambda = seq(0.0005,0.005,0.00005),
                               hold_day = 14, 
                               lag_num = lag_num, 
                               x_feature = x_feature, 
                               is.together = F, 
                               weight_type = "weighted_uniform",
                               x_pred = T, 
                               current_add = F,
                               season = 7)

model_bays <-  GetModelValTable(model = BaysianModel, 
                                is.tuning = F)

model_com_res <- rbind(model_median, model_arima, model_var, model_bays)
colnames(model_com_res) <- c(3, 7, 14)
rownames(model_com_res) <- rep(c("MSE_gcu", "MSE_memory"),4)
saveRDS(model_com_res, "tables/model_com_res.rds")

# Plots for VAR
initial_data <- data_agg_sum[as.Date(data_agg_sum$date) == (as.Date(tuning_set[1,]$date)-1), ]
var_validate <- ValidateModel(validate_type = "moving",
                              model = VarModel, 
                              tuning_set = tuning_set, 
                              validation_set = validation_set,
                              validation_interval = 14,
                              metric = "MSE", 
                              is.plot = TRUE, 
                              is.origin = T, 
                              transform.fun = Backtransform, 
                              initial_data = initial_data,
                              tuning_interval = 7, 
                              lambda = seq(0.00005,0.005,0.00005), 
                              hold_day = 14, 
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

### Check True Prediction ### 
var_pred <- ValidateModel(validate_type = "moving",
                          model = VarModel, 
                          tuning_set = train_set_all,
                          validation_set = holddout_set[holddout_set$date <= as.POSIXct("2018-03-27", tz = "America/Los_Angeles"),], 
                          validation_interval = 14,
                          metric = "MSE", 
                          is.plot = TRUE, 
                          is.origin = T, 
                          transform.fun = Backtransform, 
                          initial_data = initial_data,
                          tuning_interval = 7, 
                          lambda = seq(0.000025,0.005,0.000025), 
                          hold_day = 30, 
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

var_pred$MSE$gcu
var_pred$MSE$mem

### Prediction the contextual GCU and Memory ### 
# Get prediction
true_trainset_all <- rbind(train_set_all, 
                           holddout_set[holddout_set$date <= as.POSIXct("2018-03-27", tz = "America/Los_Angeles"),])
var_pred_est <- ValidateModel(validate_type = "moving",
                              model = VarModel, 
                              tuning_set = true_trainset_all,
                              validation_set = comparison_set, 
                              validation_interval = 14,
                              metric = "MSE", 
                              is.plot = TRUE, 
                              is.origin = T, 
                              transform.fun = Backtransform, 
                              initial_data = initial_data,
                              tuning_interval = 7, 
                              lambda = seq(0.0025,0.3,0.0025), 
                              hold_day = 28, 
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
var_pred_est$MSE$gcu
var_pred_est$MSE$mem
str(var_pred_est$model)

### Analyse the prediction of contextual GCU and Memory ### 
# Get the change rate
prediction_res_total <- rbind(var_pred$prediction_org, var_pred_est$prediction_org)
actual_data <- data_agg_sum[as.POSIXct("2018-03-28")<=data_agg_sum$date &
                              data_agg_sum$date<= as.POSIXct("2018-04-10"), c("gcu_seconds", "memory_gib_seconds", "date")] 

pred_data <- var_pred_est$prediction_org
sum(pred_data$gcu_seconds - actual_data$gcu_seconds) / sum(pred_data$gcu_seconds) # 0.02237161
sum(pred_data$memory_gib_seconds - actual_data$memory_gib_seconds) / sum(pred_data$memory_gib_seconds) #-0.001271735

# Zoom in plots
features <- c("gcu_seconds", "memory_gib_seconds")
actual_set <- data_agg_sum[2:nrow(data_agg_sum), ]
var_validate_res <- var_validate$prediction_org
var_validate_ind <- as.Date(actual_set$date) < as.Date(var_validate_res$date[1])
var_validate_res <- rbind(actual_set[var_validate_ind, c(features, "date")], 
                          var_validate_res)

var_pred_res <- var_pred$prediction_org
var_pred_ind <- as.Date(actual_set$date) < as.Date(var_pred_res$date[1])
var_pred_res <- rbind(actual_set[var_pred_ind, c(features, "date")], 
                      var_pred_res)

var_pred_est_res <- var_pred_est$prediction_org
var_pred_est_ind <- as.Date(actual_set$date) < as.Date(var_pred_est_res$date[1])
var_pred_est_res <- rbind(actual_set[var_pred_est_ind, c(features, "date")], 
                          var_pred_est_res)


GetZoomIn(dataset = var_validate_res, 
          actual_set = actual_set, 
          interval = c((nrow(var_validate_res) - 42 - 29): nrow(var_validate_res)),
          breakline = as.POSIXct("2018-01-14"),
          features_name = features) 

GetZoomIn(dataset = var_pred_res, 
          actual_set = actual_set, 
          interval = c((nrow(var_pred_res) - 30 - 29): nrow(var_pred_res)),
          breakline = as.POSIXct("2018-02-25"),
          features_name = features) 

GetZoomIn(dataset = var_pred_est_res, 
          actual_set = actual_set, 
          interval = c((nrow(var_pred_est_res) - 14 - 29): nrow(var_pred_est_res)),
          breakline = as.POSIXct("2018-03-28"),
          features_name = features) 

# Get cofidence intervals
model_fit = var_pred_est$model[[1]]
prediction_org <- var_pred_est$prediction_org
dataset = true_trainset_all[(season + 1):nrow(true_trainset_all),]
data_trans <- data_agg_sum_trans_log
data_org <- data_agg_sum

ci_res <- GetConf(model_fit, 
                  dataset, 
                  data_trans,
                  data_org,
                  type = "var", 
                  lag_num = 20, 
                  interval = 14, 
                  features = x_feature, 
                  transform.fun = Backtransform,
                  CI = c(0.25, 0.95),
                  prediction_org,
                  x_pred = T,
                  is.together = F,
                  current_add = F) 

par(mfrow = c(1,1))
plot(prediction_org$date, prediction_org$gcu_seconds, type = 'l', 
     ylim = c(min(c(ci_res$CI_day[[1]][,1], actual_data$gcu_seconds)), max(ci_res$CI_day[[1]][,2])),
     main = "Daily Confidence Interval", xlab = "Time", ylab = "gcu_seconds")
lines(prediction_org$date, ci_res$CI_day[[1]][,1], col = 3, lty = 2)
lines(prediction_org$date, ci_res$CI_day[[1]][,2], col = 3, lty = 2)
lines(actual_data$date, actual_data$gcu_seconds, col = 2)
sum(actual_data$gcu_seconds)
sum(prediction_org$gcu_seconds)
ci_res$CI_sum[[1]]

plot(prediction_org$date, prediction_org$memory_gib_seconds, type = 'l', 
     ylim = c(min(ci_res$CI_day[[2]][,1]), max(ci_res$CI_day[[2]][,2])),
     main = "Daily Confidence Interval", xlab = "Time", ylab = "memory_gib_seconds")
lines(prediction_org$date, ci_res$CI_day[[2]][,1], col = 3, lty = 2)
lines(prediction_org$date, ci_res$CI_day[[2]][,2], col = 3, lty = 2)
lines(actual_data$date, actual_data$memory_gib_seconds, col = 2)
sum(actual_data$memory_gib_seconds)
sum(prediction_org$memory_gib_seconds)
ci_res$CI_sum[[2]]


