rm(list = ls())

# Import packages ---------------------------
library(data.table)

# Load and format data ---------------------------
# Get files' name if there are many files
file_pattern <- "^data Aggregated Cell ([0-9]+){4}.csv$"
files <- list.files(path = "Efficiency Analysis/", 
                    pattern = file_pattern, 
                    full.names = T)
data_agg_ls <- lapply(1:length(files), function(x) {
  data.table::data.table(read.csv(files[x], header = T))
})
data_agg_total <- as.data.frame(data.table::rbindlist(data_agg_ls, fill = T))


# Time variable needs to be convert from factor to date format
data_agg_total$date <- strptime(as.character(data_agg_total$date), "%Y-%m-%d")
# Hour variable needs to be combined with date
date_hour = floor(data_agg_total$hour) 
date_min = ((data_agg_total$hour * 3600) %% 3600) %/% 60
date_second = floor(((data_agg_total$hour * 3600) %% 3600) %% 60)
date_total = paste(as.character(data_agg_total$date), " ", 
                   date_hour, ":", 
                   date_min, ":", 
                   date_second, sep = "")



data_agg_total$date <- strptime(as.character(date_total), 
                                "%Y-%m-%d %H:%M:%S", 
                                tz = "GMT")


data_agg_total <- data_agg_total[,-2]

# # Check if the data file is correct
# lapply(1:length(data_agg_ls),function(x) head(data_agg_ls[[x]]))
# data_2020 = read.csv(files[[5]], header = T)
# head(data_2020)
# data_2016 = read.csv(files[[1]], header = T)
# head(data_2016)

# Save data to rds file, which can be read faster
saveRDS(data_agg_total, "data_agg_total.rds")
data_agg_total <- readRDS("data_agg_total.rds")