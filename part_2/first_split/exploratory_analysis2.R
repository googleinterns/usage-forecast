rm(list = ls())
<<<<<<< HEAD
setwd("/Volumes/GoogleDrive/My Drive/alsta_analysis_LA")
source("functions_v3.R")
=======
source("functions.R")
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a

############################################
##### Section 1: Read and Correct data #####
############################################
<<<<<<< HEAD
data_agg <- read.csv("data/Veritas_all_LA_v2.csv")
=======
data_agg <- read.csv("data/Veritas_all_LA.csv")
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
summary(data_agg)

### Correct data ###
# Check infinity, na
inf_ind <- which(data_agg$memory_gib_seconds == "Inf")
data_agg[inf_ind,"memory_gib_seconds"] <- 
  mean(data_agg[inf_ind-1,"memory_gib_seconds"] + 
         data_agg[inf_ind+1,"memory_gib_seconds"])

na_ind <- which(is.na(data_agg$memory_gib_seconds))
data_agg[na_ind,"memory_gib_seconds"] <- 
  mean(data_agg[na_ind-1,"memory_gib_seconds"] + 
         data_agg[na_ind+1,"memory_gib_seconds"])
na_ind_gpu <- which(is.na(data_agg$gpu))

na_ind <- which(is.na(data_agg$gcu_seconds))
data_agg <- data_agg[-na_ind, ]

na_ind_gpu <- which(is.na(data_agg$gpu))

# Check whether na happens before 03-28-2018
<<<<<<< HEAD
change_date <- as.Date("2018-03-28", tz = "America/Los_Angeles")
data_agg[as.Date(data_agg[na_ind_gpu,"date"], tz = "America/Los_Angeles") <= 
           change_date,]
data_agg_total <- data_agg
=======
change_date <- as.POSIXct("2018-03-28")
data_agg[as.POSIXct(data_agg[na_ind_gpu,"date"], tz = "America/Los_Angeles") <= 
           change_date,]
data_agg_total <- data_agg[!IsDash(data_agg$cell),]
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a

# Correct data type
data_agg_total$cell <- factor(data_agg_total$cell, unique(data_agg_total$cell))
data_agg_total$date <- as.Date(data_agg_total$date, tz = "America/Los_Angeles")

### Summary statistics ###
(n <- dim(data_agg_total)[1])
(k <- dim(data_agg_total)[2])
tail(data_agg_total)
str(data_agg_total)
summary(data_agg_total)

### Select the useful data ###
## Find the cells that both happen before the change and after the change.
# Get the index of the cell that starts after change date.
data_agg_total <- GetTimeInfo(data_agg_total)
ind_cell_after18 <- IsAfterDate(data_agg_total, "cell", change_date)

# Get the cell name that before the change date.
cell_before18 <- ind_cell_after18$`get(group)`[-ind_cell_after18$res]
data_agg_total <- data_agg_total[data_agg_total$cell %in% cell_before18, ]
saveRDS(data_agg_total, "data_agg_total.rds")
data_agg_total <- readRDS("data_agg_total.rds")

# Remove the error data.
<<<<<<< HEAD
error_start_date <- as.Date("2017-08-01", tz = "America/Los_Angeles")
error_end_date <- as.Date("2017-09-02", tz = "America/Los_Angeles")
data_agg_clean <- data_agg_total[data_agg_total$date > error_end_date | 
                                   data_agg_total$date < error_start_date, ]
# Get the data one month after the change date.
# data_agg_clean <- data_agg_clean[data_agg_clean$date <= as.Date("2018-04-28", tz = "America/Los_Angeles"), ]
=======
data_agg_clean <- data_agg_total[data_agg_total$date > as.POSIXct("2017-09-02", tz = "America/Los_Angeles") | 
                                   data_agg_total$date < as.POSIXct("2017-08-01", tz = "America/Los_Angeles"), ]
# Get the data one month after the change date.
data_agg_clean <- data_agg_clean[data_agg_clean$date <= as.POSIXct("2018-04-28", tz = "America/Los_Angeles"), ]
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
saveRDS(data_agg_clean, "data_agg_clean.rds")
data_agg_clean <- readRDS("data_agg_clean.rds")

###################################
##### Section 2: Visulazation #####
###################################

### Line plots for original time series ###
GetLineplot(dataset = data_agg_total, "cell", filepath = "plots/org_") 
# Remove extreme points
GetLineplot(data_agg_clean, "cell", "./plots/clean_")
GetLineplot(data_agg_clean[data_agg_clean$cell == "os",], "cell", "./plots/os_")

### Density plots for time series after the change date ###
<<<<<<< HEAD
GetDensityplot(data_agg_clean, "cell", "gcu_seconds", "./plots/", "GCU-Seconds")
=======
GetDensityplot(data_agg_clean, "cell", "gcu_seconds", "./plots/")
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
# The plots are skewed to the right, so we need to check the cells that skewed a lot.
summary(data_agg_clean[data_agg_clean$gcu_seconds > 400,"cell"])
# Remove the cells that has extreme values.
ind_tmp <- data_agg_clean$cell %in% c("qf", "ij")
data_agg_clean_tmp <- data_agg_clean[!ind_tmp, ]
GetDensityplot(data_agg_clean_tmp, "cell", "gcu_seconds", "./plots/rm400_", "GCU-Seconds")
# Still skewed, take log log
data_agg_clean_tmp$gcu_seconds <- (data_agg_clean_tmp$gcu_seconds)^(1/20)
<<<<<<< HEAD
GetDensityplot(data_agg_clean_tmp, "cell", "gcu_seconds", "./plots/rm400_sq20_", "GCU-Seconds")
=======
GetDensityplot(data_agg_clean_tmp, "cell", "gcu_seconds", "./plots/rm400_sq20_")
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a

# Get Density plots and MA plots for all years
features_name <- c("Gpu-Seconds", "GCU-Seconds", "RAM (GiB)-Seconds")
features <- c("gpu", "gcu_seconds", "memory_gib_seconds")
for (i in 1:length(features_name)) {
  # Density plots with scales
  data_agg_clean_tmp <- data_agg_clean
  data_agg_clean_tmp[, features[i]] <- (data_agg_clean[, features[i]])^(1/20)
  GetDensityplot(dataset = data_agg_clean_tmp, group = "cell", 
                 feature = features[i], filepath = paste("./plots/root20_", sep = ""),
                 feature_lab = features_name[i])
  
  # MA plots
  GetMAplot(dataset = data_agg_clean, feature = features[i], 
            group = "cell", ma_order = 30, 
            filepath = paste("./plots/ma30_", sep = ""), 
            feature_lab = features_name[i])
  GetMAplot(dataset = data_agg_clean, feature = features[i], 
            group = "cell", ma_order = 14, 
            filepath = paste("./plots/ma14_", sep = ""),
            feature_lab = features_name[i])
  GetMAplot(dataset = data_agg_clean, feature = features[i], 
            group = "cell", ma_order = 7, 
            filepath = paste("./plots/ma7_", sep = ""),
            feature_lab = features_name[i])
}


### Plots for total usage ###
# Get total usage for all cells
<<<<<<< HEAD
data_agg_sum <- ddply(data_agg_total, .(date), summarize, gcu_seconds = sum(gcu_seconds), 
=======
data_agg_sum <- ddply(data_agg_total, .(date), summarize,gcu_seconds = sum(gcu_seconds), 
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
                              memory_gib_seconds = sum(memory_gib_seconds),
                              gpu = sum(gpu)
)
data_agg_sum$cell <- "total"
data_agg_sum <- GetTimeInfo(data_agg_sum)
saveRDS(data_agg_sum, "data_agg_sum.rds")
<<<<<<< HEAD

data_agg_clean_total <- data_agg_sum[data_agg_sum$date > error_end_date | 
                                       data_agg_sum$date < error_start_date, ]
# change_date_1monthaft <- as.Date("2018-04-28", tz = "America/Los_Angeles")
# data_agg_clean_total <- data_agg_clean_total[data_agg_clean_total$date <= change_date_1monthaft,]
=======
data_agg_clean_total <- data_agg_sum[data_agg_sum$date > as.POSIXct("2017-09-02", tz = "America/Los_Angeles") | 
                                       data_agg_sum$date < as.POSIXct("2017-08-01", tz = "America/Los_Angeles"), ]
data_agg_clean_total <- data_agg_clean_total[data_agg_clean_total$date <= as.POSIXct("2018-04-28", tz = "America/Los_Angeles"),]
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a

### Line plots for total usage ###
GetLineplot(dataset = data_agg_clean_total, "cell", filepath = "plots/total_") 
GetLineplot(dataset = data_agg_sum, "cell", filepath = "plots/org_total_") 

### Get Density plots and MA plots for total usage ###
<<<<<<< HEAD
for (i in 1:length(features_name)) {
=======
for (i in features_name) {
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
  # Density plots with scales
  data_agg_clean_tmp <- data_agg_clean_total
  data_agg_clean_tmp[,features[i]] <- (data_agg_clean_total[,features[i]])^(1/20)
  GetDensityplot(dataset = data_agg_clean_tmp, group = "cell", 
                 feature = features[i], 
                 filepath = paste("./plots/total_root20_", sep = ""),
                 feature_lab = features_name[i])
  
  # MA plots
  GetMAplot(dataset = data_agg_clean_total, feature = features[i], 
            group = "cell", ma_order = 30, 
            filepath = paste("./plots/total_ma30_", sep = ""),
            feature_lab = features_name[i])
  GetMAplot(dataset = data_agg_clean_total, feature = features[i], 
            group = "cell", ma_order = 14, 
            filepath = paste("./plots/total_ma14_", sep = ""),
            feature_lab = features_name[i])
  GetMAplot(dataset = data_agg_clean_total, feature = features[i], 
            group = "cell", ma_order = 7, 
            filepath = paste("./plots/total_ma7_", sep = ""),
            feature_lab = features_name[i])
}

# Clutering time series and visulaze
# Don't have time to finish
data_agg_clean_cl <- GetClusterPrep(dataset = data_agg_clean, features_name = features_name, obs = "cell", key = "date") 

# Remove 2016 data, since 2016 is potentially differ from other years
data_agg_clean_2016_rm <- data_agg_clean[data_agg_clean$Year != "16",]
<<<<<<< HEAD
for (i in 1:length(features_name)) {
=======
for (i in features_name) {
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
  # Density plots with scales
  data_agg_clean_tmp <- data_agg_clean_2016_rm
  data_agg_clean_tmp[,i] <- (data_agg_clean_2016_rm[,i])^(1/20)
  GetDensityplot(dataset = data_agg_clean_tmp, 
                 group = "cell", feature = features[i], 
                 filepath = paste("./plots/root20_16rm_",i,"_",sep = ""),
                 feature_lab = features_name[i])
  
}

######################################
##### Section 3.1: Tranformation #####
######################################

### Tranformation 1: take differences ###
<<<<<<< HEAD
data_agg_total_trans <- as.data.frame(sapply(features, function(x) {
=======
data_agg_total_trans <- as.data.frame(sapply(features_name, function(x) {
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
  data_agg_clean_total[2:dim(data_agg_clean_total)[1], x] - data_agg_clean_total[1:(dim(data_agg_clean_total)[1]-1), x]}))
data_agg_total_trans$date <- data_agg_clean_total$date[2:dim(data_agg_clean_total)[1]]
data_agg_total_trans$cell <- "total"
data_agg_trans_2016_rm <- data_agg_total_trans[format(data_agg_total_trans$date,"%Y") != "2016", ]

### Check the difference of time series ###
<<<<<<< HEAD
breaktime <- as.Date("2017-08-01", tz = "America/Los_Angeles")
=======
breaktime <- as.POSIXct("2017-08-01", tz = "America/Los_Angeles")
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
GetLineplot(dataset = data_agg_total_trans[data_agg_total_trans$date < breaktime,], "cell", filepath = "plots/transform/diff_") 
GetLineplot(dataset = data_agg_trans_2016_rm[data_agg_trans_2016_rm$date < breaktime,], "cell", filepath = "plots/transform/rm_diff_") 

### Tranformation 2: take log ###
<<<<<<< HEAD
data_agg_total_trans_log <- as.data.frame(sapply(features, function(x) {
=======
data_agg_total_trans_log <- as.data.frame(sapply(features_name, function(x) {
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
  log(data_agg_clean_total[2:dim(data_agg_clean_total)[1], x]) - log(data_agg_clean_total[1:(dim(data_agg_clean_total)[1]-1), x])}))
data_agg_total_trans_log$date <- data_agg_clean_total$date[2:dim(data_agg_clean_total)[1]]
data_agg_total_trans_log$cell <- "total"
data_agg_trans_log_2016_rm <- data_agg_total_trans_log[format(data_agg_total_trans_log$date,"%Y") != "2016", ]

# Check the difference of time series
GetLineplot(dataset = data_agg_total_trans_log[data_agg_total_trans_log$date < breaktime,], "cell", filepath = "plots/transform/log_") 
GetLineplot(dataset = data_agg_trans_log_2016_rm[data_agg_trans_log_2016_rm$date < breaktime,], "cell", filepath = "plots/transform/rm_log_") 

### Tranformation 3: take increase rate ###
<<<<<<< HEAD
data_agg_total_trans_rate <- as.data.frame(sapply(features, function(x) {
=======
data_agg_total_trans_rate <- as.data.frame(sapply(features_name, function(x) {
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
  (data_agg_clean_total[2:dim(data_agg_clean_total)[1], x] - data_agg_clean_total[1:(dim(data_agg_clean_total)[1]-1), x]) / 
    data_agg_clean_total[1:(dim(data_agg_clean_total)[1]-1), x]}))
data_agg_total_trans_rate$date <- data_agg_clean_total$date[2:dim(data_agg_clean_total)[1]]
data_agg_total_trans_rate$cell <- "total"
data_agg_trans_rate_2016_rm <- data_agg_total_trans_rate[format(data_agg_total_trans_rate$date,"%Y") != "2016", ]

# Check the difference of time series
GetLineplot(dataset = data_agg_total_trans_rate[data_agg_total_trans_rate$date < breaktime,], "cell", filepath = "plots/transform/rate_") 
GetLineplot(dataset = data_agg_trans_rate_2016_rm[data_agg_trans_rate_2016_rm$date < breaktime,], "cell", filepath = "plots/transform/rm_rate_") 

### Tranformation 4: take log increase rate ###
<<<<<<< HEAD
data_agg_total_trans_lograte <- as.data.frame(sapply(features, function(x) {
=======
data_agg_total_trans_lograte <- as.data.frame(sapply(features_name, function(x) {
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
  (log(data_agg_clean_total[2:dim(data_agg_clean_total)[1], x]) - log(data_agg_clean_total[1:(dim(data_agg_clean_total)[1]-1), x])) / 
    log(data_agg_clean_total[1:(dim(data_agg_clean_total)[1]-1), x])}))
data_agg_total_trans_lograte$date = data_agg_clean_total$date[2:dim(data_agg_clean_total)[1]]
data_agg_total_trans_lograte$cell <- "total"
data_agg_trans_lograte_2016_rm = data_agg_total_trans_lograte[format(data_agg_total_trans_lograte$date,"%Y") != "2016", ]

### Tranformation 2 for different cells ###
data_agg_clean_trans <- NULL
for (i in unique(data_agg_clean$cell)) {
  data_agg_clean_i <- data_agg_clean[data_agg_clean$cell == i,]
<<<<<<< HEAD
  trans_log <- as.data.frame(matrix(sapply(features, function(x) {
    (log(data_agg_clean_i[2:dim(data_agg_clean_i)[1], x]) - log(data_agg_clean_i[1:(dim(data_agg_clean_i)[1]-1), x])) }), ncol = length(features),
    byrow = F))
=======
  trans_log <- as.data.frame(sapply(features_name, function(x) {
    (log(data_agg_clean_i[2:dim(data_agg_clean_i)[1], x]) - log(data_agg_clean_i[1:(dim(data_agg_clean_i)[1]-1), x])) }))
>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
  trans_log$date <- data_agg_clean_i$date[2:nrow(data_agg_clean_i)]
  trans_log$cell <- i
  data_agg_clean_trans <- rbind(data_agg_clean_trans, trans_log)
}
<<<<<<< HEAD
colnames(data_agg_clean_trans) <- c(features, "date", "cell")
=======

>>>>>>> 882f2a7cc176ef73410b92c1ee4f305687eea75a
data_agg_clean_trans_2016_rm <- data_agg_clean_trans[format(data_agg_clean_trans$date,"%Y") != "2016", ]

##############################################
##### Section 3.2: Analyse Tranformation #####
##############################################

### Visulazation of the transformed data ###
# Get line plots for transformed data.
GetLineplot(dataset = data_agg_total_trans_lograte[data_agg_total_trans_lograte$date < breaktime,], "cell", filepath = "plots/transform/lograte_") 
GetLineplot(dataset = data_agg_trans_lograte_2016_rm[data_agg_trans_lograte_2016_rm$date < breaktime,], "cell", filepath = "plots/transform/rm_lograte_") 

data_agg_1day_trans_log <- data_agg_total_trans_log
saveRDS(data_agg_total_trans_log, "data_agg_total_trans_log.rds")

# Check correlation between time series.
# Before transformation.
plot(data_agg_clean_total$gcu_seconds, data_agg_clean_total$memory_gib_seconds, type = "l")
cor(data_agg_clean_total$gcu_seconds, data_agg_clean_total$memory_gib_seconds) # 0.9854569

# After transformation.
plot(data_agg_total_trans_log$gcu_seconds[-1], data_agg_total_trans_log$memory_gib_seconds[-1])
cor(data_agg_total_trans_log$gcu_seconds[-1], data_agg_total_trans_log$memory_gib_seconds[-1]) # 0.05547329

### Check stationarity of the transformed data ###
# Check stationarity with time before 2017-07-30
data_agg_1day_trans_log_before <- data_agg_total_trans_log[data_agg_total_trans_log$date < breaktime,]
data_agg_1day_trans_log_before <- data_agg_1day_trans_log_before[-1,]
# Since gpu is NA for data before 2017-07-30
features <- features[-1]

pval <- c()
for (i in features) {
  data_before_ind <- is.na(data_agg_1day_trans_log_before[,i])
  print(i)
  pval[i] <- tseries::adf.test(data_agg_1day_trans_log_before[!data_before_ind,i], alternative = "stationary")$p.value
  acf(data_agg_1day_trans_log_before[!data_before_ind,i])
  pacf(data_agg_1day_trans_log_before[!data_before_ind,i])
}
saveRDS(pval, "tables/stationarity_trans.rds")

# Check stationarity for original data
data_agg_1day_before <- data_agg_clean_total[data_agg_clean_total$date < breaktime,]
data_agg_1day_before <- data_agg_1day_before[-1,]
pval <- c()
for (i in features) {
  pval[i] <- tseries::adf.test(data_agg_1day_before[,i], alternative = "stationary")$p.value
  acf(data_agg_1day_trans_log_before[,i])
  pacf(data_agg_1day_trans_log_before[,i])
}
saveRDS(pval, "tables/stationarity_before1708_16rm.rds")


### Visulazation of the transformed data for different cells ###
# Check the difference of time series
GetLineplot(dataset = data_agg_clean_trans[data_agg_clean_trans$date < breaktime,], "cell", filepath = "plots/transform/alllog_") 
GetLineplot(dataset = data_agg_clean_trans_2016_rm[data_agg_clean_trans_2016_rm$date < breaktime,], "cell", filepath = "plots/transform/allrm_log_") 

### Check stationarity of the transformed data for different cells ###
## Check stationarity
features_name <- c("gcu_seconds", 
                   "memory_gib_seconds")
unique_cell <- unique(data_agg_clean_trans$cell)
res_stationary <- matrix(0, nrow = length(features_name), ncol = length(unique_cell))
for (i in 1:length(features_name)) {
  for (j in 1:length(unique_cell)) {
    data_agg_clean_trans_j <- data_agg_clean_trans[data_agg_clean_trans$cell == unique_cell[j],]
    data_before_ind <- is.na(data_agg_clean_trans_j[,features_name[i]])
    res_stationary[i, j] <- tseries::adf.test(data_agg_clean_trans_j[!data_before_ind, features_name[i]], alternative = "stationary")$p.value
  }
}

data_agg_1day_before <- data_agg_clean_total[data_agg_clean_total$date < breaktime,]
data_agg_1day_before <- data_agg_1day_before[-1,]
pval <- c()
for (i in features_name) {
  pval[i] <- tseries::adf.test(data_agg_1day_before[,i], alternative = "stationary")$p.value
  acf(data_agg_1day_trans_log_before[,i])
  pacf(data_agg_1day_trans_log_before[,i])
}

