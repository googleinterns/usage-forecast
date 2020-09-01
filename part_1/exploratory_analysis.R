rm(list = ls())
source("functions.R")

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

# Read data---------------------------
data_agg_total <- readRDS("data_agg_total.rds")

# Summary statistics---------------------------
(n <- dim(data_agg_total)[1])
(k <- dim(data_agg_total)[2])
head(data_agg_total)
str(data_agg_total)
summary(data_agg_total)

# Create minutes, hour, day, week, month, year variables to summarize
features_select <- c("feature1", "feature2", "feature3_seconds", 
                    "feature4_gib_seconds", "disk_gib_seconds")

info_table <- lapply(features_select, function(x) GetSummaryTable(dataset = data_agg_total, 
                                                               feature = x,
                                                               time_window = 15))
# # check na
# lapply(1:length(info_table), function(x) info_table[[x]][is.na(info_table[[x]]),])

# saveRDS(info_table, "info_table.rds")
info_table <- readRDS("info_table.rds")
info_table_clean <- lapply(1:length(info_table), function(x) 
  info_table[[x]][(info_table[[x]]$Month != "Aug" | info_table[[x]]$Year != "17"),])
info_table_clean <- lapply(1:length(info_table_clean), function(x) 
  info_table_clean[[x]][(info_table_clean[[x]]$Month != "Mar" | info_table_clean[[x]]$Year != "19" |
                           info_table_clean[[x]]$Day != "19"),])

summary_bytime <- lapply(1:length(info_table_clean), function(x) GetSummary(info_table_clean[[x]]))

# Visulazation ---------------------------
# Line plots for original time series
GetLineplot(dataset = data_agg_total, filepath = "plots/org_") 

# Remove extreme points
data_agg_total_clean = data_agg_total[as.Date(data_agg_total$date) != as.Date("2019-03-19", tz = "GMT"), ]
data_agg_total_clean = data_agg_total_clean[format(data_agg_total_clean$date,"%Y-%m") != "2017-08", ]
GetLineplot(dataset = data_agg_total_clean, filepath = "plots/rm_") 

# Get Boxplots and Bar charts for all years
features_name = c("feature1", "feature2",
                  "feature3", "feature4")
for (i in 1:length(features_name)) {
  GetBoxplot(dataset = info_table_clean[[i]], 
             feature = features_name[i],
             summary_stat = "sum",
             filepath = "plots/")
  GetBarplot(dataset = info_table_clean[[i]], 
             feature = features_name[i],
             summary_stat = "sum",
             filepath = "plots/")
  GetDensityplot(dataset = info_table_clean[[i]], 
             feature = features_name[i],
             summary_stat = "sum",
             filepath = "plots/")
}

for (i in 1:length(features_name)) {
  GetBoxplot(dataset = info_table_clean[[i]], 
             feature = features_name[i],
             summary_stat = "upper",
             filepath = "plots/upper/")
  GetBarplot(dataset = info_table_clean[[i]], 
             feature = features_name[i],
             summary_stat = "upper",
             filepath = "plots/upper/")
  GetDensityplot(dataset = info_table_clean[[i]], 
                 feature = features_name[i],
                 summary_stat = "upper",
                 filepath = "plots/upper/")
}

# Moving average plots with all years
ma_ggplot = list()
for (i in 1:length(features_name)) {
  ma_ggplot[[i]] = GetMAplot(dataset = info_table_clean[[i]], 
            feature = features_name[i],
            summary_stat = "sum",
            ma_order = c(4, 4*24, 4*24*7, 4*24*30))
}

ggsave(filename = paste("plots/", "allfeatures_maplot.png", sep = ""), 
       plot = grid.arrange(ma_ggplot[[1]], 
                         ma_ggplot[[2]],
                         ma_ggplot[[3]],
                         ma_ggplot[[4]]),
       width = 10, height = 8, dpi = 100)

ma_ggplot <- list()
for (i in 1:length(features_name)) {
  ma_ggplot[[i]] = GetMAplot(dataset = info_table_clean[[i]], 
                             feature = features_name[i],
                             summary_stat = "upper",
                             ma_order = c(4, 4*24, 4*24*7, 4*24*30))
}

ggsave(filename = paste("plots/upper/", "allfeatures_maplot.png", sep = ""), 
       plot = grid.arrange(ma_ggplot[[1]], 
                         ma_ggplot[[2]],
                         ma_ggplot[[3]],
                         ma_ggplot[[4]]),
       width = 10, height = 8, dpi = 100)

# Remove 2016 data, since 2016 differ from other years
info_table_2016_rm <- lapply(info_table_clean, function(x) x[x$Year != "16", ])
for (i in 1:length(features_name)) {
  GetBoxplot(dataset = info_table_2016_rm[[i]], 
             feature = features_name[i],
             summary_stat = "upper",
             filepath = "plots/2016_rm/")
  GetBarplot(dataset = info_table_2016_rm[[i]], 
             feature = features_name[i],
             summary_stat = "upper",
             filepath = "plots/2016_rm/")
  GetDensityplot(dataset = info_table_2016_rm[[i]], 
                 feature = features_name[i],
                 summary_stat = "upper",
                 filepath = "plots/2016_rm/")
  GetBoxplot(dataset = info_table_2016_rm[[i]], 
             feature = features_name[i],
             summary_stat = "sum",
             filepath = "plots/2016_rm/")
  GetBarplot(dataset = info_table_2016_rm[[i]], 
             feature = features_name[i],
             summary_stat = "sum",
             filepath = "plots/2016_rm/")
  GetDensityplot(dataset = info_table_2016_rm[[i]], 
                 feature = features_name[i],
                 summary_stat = "sum",
                 filepath = "plots/2016_rm/")
}

ma_ggplot <- list()
for (i in 1:length(features_name)) {
  ma_ggplot[[i]] = GetMAplot(dataset = info_table_clean[[i]], 
                             feature = features_name[i],
                             summary_stat = "upper",
                             ma_order = c(4, 4*24, 4*24*7, 4*24*30))
}

ggsave(filename = paste("plots/2016_rm/", "allfeatures_maplot.png", sep = ""), 
       plot = grid.arrange(ma_ggplot[[1]], 
                         ma_ggplot[[2]],
                         ma_ggplot[[3]],
                         ma_ggplot[[4]]),
       width = 10, height = 8, dpi = 100)

# Clean data---------------------------
## Tranformation 1: take differences
data_agg_total_trans <- as.data.frame(sapply(features_select, function(x) {
  data_agg_total_clean[2:dim(data_agg_total_clean)[1], x] - data_agg_total_clean[1:(dim(data_agg_total_clean)[1]-1), x]}))
data_agg_total_trans$date <- data_agg_total_clean$date[2:dim(data_agg_total_clean)[1],]
data_agg_trans_2016_rm <- data_agg_total_trans[format(data_agg_total_trans$date,"%Y") != "2016", ]

## Tranformation 2: take differences of log
data_agg_total_trans_log <- as.data.frame(sapply(features_select, function(x) {
  log(data_agg_total_clean[2:dim(data_agg_total_clean)[1], x]) - log(data_agg_total_clean[1:(dim(data_agg_total_clean)[1]-1), x])}))
data_agg_total_trans_log$date <- data_agg_total_clean$date[2:dim(data_agg_total_clean)[1],]
data_agg_trans_log_2016_rm <- data_agg_total_trans_log[format(data_agg_total_trans_log$date,"%Y") != "2016", ]
                                                                        
# Check the difference of time series
GetLineplot(dataset = data_agg_total_trans_log, filepath = "plots/transform/log_") 
GetLineplot(dataset = data_agg_trans_log_2016_rm, filepath = "plots/transform/rm_log_") 

## Tranformation 3: take increase rate
data_agg_total_trans_rate <- as.data.frame(sapply(features_select, function(x) {
  (data_agg_total_clean[2:dim(data_agg_total_clean)[1], x] - data_agg_total_clean[1:(dim(data_agg_total_clean)[1]-1), x]) / 
    data_agg_total_clean[1:(dim(data_agg_total_clean)[1]-1), x]}))
data_agg_total_trans_rate$date <- data_agg_total_clean$date[2:dim(data_agg_total_clean)[1],]
data_agg_trans_rate_2016_rm <- data_agg_total_trans_rate[format(data_agg_total_trans_rate$date,"%Y") != "2016", ]

# Check the difference of time series
GetLineplot(dataset = data_agg_total_trans_rate, filepath = "plots/transform/rate_") 
GetLineplot(dataset = data_agg_trans_rate_2016_rm, filepath = "plots/transform/rm_rate_") 

## Tranformation 4: take log increase rate
data_agg_total_trans_lograte <- as.data.frame(sapply(features_select, function(x) {
  (log(data_agg_total_clean[2:dim(data_agg_total_clean)[1], x]) - log(data_agg_total_clean[1:(dim(data_agg_total_clean)[1]-1), x])) / 
    log(data_agg_total_clean[1:(dim(data_agg_total_clean)[1]-1), x])}))
data_agg_total_trans_lograte$date = data_agg_total_clean$date[2:dim(data_agg_total_clean)[1],]
data_agg_trans_lograte_2016_rm = data_agg_total_trans_lograte[format(data_agg_total_trans_lograte$date,"%Y") != "2016", ]

# Check the difference of time series
GetLineplot(dataset = data_agg_total_trans_lograte, filepath = "plots/transform/lograte_") 
GetLineplot(dataset = data_agg_trans_lograte_2016_rm, filepath = "plots/transform/rm_lograte_") 

## Take differences of log for the one day aggregation
info_table_1day <- lapply(features_select, function(x) GetSummaryTable(dataset = data_agg_total, 
                                                                                      feature = x,
                                                                                      time_window = 60*24))
# saveRDS(info_table_1day, "info_table_1day.rds")
info_table_1day <- readRDS("info_table_1day.rds")
info_table_1day_clean <- lapply(info_table_1day, function(x) 
  x[(x$Month != "Aug" | x$Year != "17"),])
info_table_1day_clean <- lapply(info_table_1day_clean, function(x) 
  x[(x$Month != "Mar" | x$Year != "19" |
                           x$Day != "19"),])

info_table_1day_clean_agg <- as.data.frame(
  sapply(info_table_1day_clean, function(x) {
    x$sum
  })
)
colnames(info_table_1day_clean_agg) <- features_select
info_table_1day_clean_agg$date <- as.POSIXlt(info_table_1day_clean[[1]]$date, tz = "GMT")
saveRDS(info_table_1day_clean_agg, "info_table_1day_clean_agg.rds")

data_agg_1day_trans_log <- as.data.frame(
  sapply(info_table_1day_clean, function(x) {
    log(x$sum[2:dim(x)[1]]) - log(x$sum[1:(dim(x)[1]-1)])
  })
)
colnames(data_agg_1day_trans_log) <- features_select
data_agg_1day_trans_log$date <- as.POSIXlt(info_table_1day_clean[[1]]$date[2:dim(info_table_1day_clean[[1]])[1]], tz = "GMT")
data_agg_1day_trans_log_2016_rm <- data_agg_1day_trans_log[format(data_agg_1day_trans_log$date,"%Y") != "2016", ]

# saveRDS(data_agg_1day_trans_log, "data_agg_1day_trans_log.rds")
data_agg_1day_trans_log <- readRDS("data_agg_1day_trans_log.rds")

# Check the plots for transformed time series
GetLineplot(dataset = data_agg_1day_trans_log, filepath = "plots/transform/daily_log_")
GetLineplot(dataset = data_agg_1day_trans_log_2016_rm, filepath = "plots/transform/rm_daily_log_")

ma_ggplot <- list()
data_agg_1day_trans_log$date <- info_table_1day_clean[[1]]$date[2:dim(info_table_1day_clean[[1]])[1]]
for (i in 1:length(features_name)) {
  ma_ggplot[[i]] = GetMAplot(dataset = data_agg_1day_trans_log,
                             feature = features_name[i],
                             summary_stat = features_select[i],
                             ma_order = c(2, 7, 15, 30))
}

ggsave(filename = paste("plots/transform/", "allfeatures_maplot.png", sep = ""), 
       plot = grid.arrange(ma_ggplot[[1]], 
                         ma_ggplot[[2]],
                         ma_ggplot[[3]],
                         ma_ggplot[[4]]),
       width = 10, height = 8, dpi = 100)

tail(data_agg_1day_trans_log)

## Check stationarity with time before 2017-07-30
change_date <- as.POSIXlt("2017-07-30", tz = "GMT")
data_agg_1day_trans_log$date <- as.POSIXlt(data_agg_1day_trans_log$date,
                                          tz = "GMT")
data_agg_1day_trans_log_before <- data_agg_1day_trans_log[data_agg_1day_trans_log$date <= change_date,]

pval <- c()
for (i in features_select) {
  pval[i] <- tseries::adf.test(data_agg_1day_trans_log_before[,i], alternative = "stationary")$p.value
  acf(data_agg_1day_trans_log_before[,i])
  pacf(data_agg_1day_trans_log_before[,i])
}

print(xtable::xtable(as.data.frame(pval)), type = "html")




