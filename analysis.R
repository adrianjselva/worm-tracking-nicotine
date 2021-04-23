library(tidyverse)
library(zoo)
library(ggridges)

# Functions

# Condense vector by averaging over the values within the given interval
interval_mean <- function(master, col, interval) {
  result <- c()
  sub <- master[, c("time", col)]
  
  for(i in seq(0, round(max(master$time) - 1), by = interval)) {
    temp_interval <- sub[[col]][master$time >= i & master$time < (i + interval)]
    
    result <- c(result, mean(temp_interval, na.rm = TRUE))
  }
  
  return(result)
}


# File paths

### 0 uM -----------

# 04/01/2021
sample1_0um_path <- "Source Videos/4:1:2021/0uM_1/Results/0um_1_modified_TS_W14095_speed.csv"
sample2_0um_path <- "Source Videos/4:1:2021/0uM_2/Results/0uM_2_modified_TS_W1439_speed.csv"

### 5 uM -----------

# 03/25/2021
sample1_5um_path <- "Source Videos/03:25:2021/5uM_1/Results/vid1_TS_W1558_speed.csv"
sample2_5um_path <- "Source Videos/03:25:2021/5uM_2/Results/5um_sample2_modified_TS_W520_speed.csv"

# 04/01/2021
sample3_5um_path <- "Source Videos/4:1:2021/5uM_1/Results/5uM_1_modified_TS_W3880_speed.csv"
sample4_5um_path <- "Source Videos/4:1:2021/5uM_1/Results/5uM_1_modified_TS_W4068_speed.csv"
sample5_5um_path <- "Source Videos/4:1:2021/5uM_2/Results/5uM_2_modified_TS_W4840_speed.csv"
sample6_5um_path <- "Source Videos/4:1:2021/5uM_2/Results/5uM_2_modified_TS_W6572_speed.csv"
sample7_5um_path <- "Source Videos/4:1:2021/5uM_2/Results/5uM_2_modified_TS_W6578_speed.csv"

### 20 uM ----------

# 03/25/2021
sample1_20um_path <- "Source Videos/03:25:2021/20uM_1/Results/20um_sample1_modified_TS_W591_speed.csv"
sample2_20um_path <- "Source Videos/03:25:2021/20uM_2/Results/20um_sample2_modified_TS_W1681_speed.csv"

# 04/01/2021
sample3_20um_path <- "Source Videos/4:1:2021/20uM_1/Results/20uM_1_modified_TS_W2900_speed.csv"

### 60 uM ----------

# 03/25/2021
sample1_60um_path <- "Source Videos/03:25:2021/60uM_1/Results/60um_sample1_modified_TS_W1226_speed.csv"
sample2_60um_path <- "Source Videos/03:25:2021/60uM_2/Results/60um_sample2_modified_TS_W2090_speed.csv"

# 04/01/2021
sample3_60um_path <- "Source Videos/4:1:2021/60uM_1/Results/60uM_1_modified_TS_W3972_speed.csv"
sample4_60um_path <- "Source Videos/4:1:2021/60uM_2/Results/60uM_2_modified_TS_W7693_speed.csv"

### 200 uM ----------

# 03/25/2021
sample1_200um_path <- "Source Videos/03:25:2021/200uM_1/Results/200um_sample1_modified_TS_W2834_speed.csv"
sample2_200um_path <- "Source Videos/03:25:2021/200uM_2/Results/200um_sample2_modified_TS_W2393_speed.csv"

# 04/01/2021
sample3_200um_path <- "Source Videos/4:1:2021//200uM_1/Results/200uM_1_modified_TS_W1356_speed.csv"
sample4_200um_path <- "Source Videos/4:1:2021//200uM_2/Results/200uM_2_modified_TS_W3273_speed.csv"

# Load .csv data

s1_0 <- read.csv(sample1_0um_path)
s2_0 <- read.csv(sample2_0um_path)

s1_5 <- read.csv(sample1_5um_path)
s2_5 <- read.csv(sample2_5um_path)
s3_5 <- read.csv(sample3_5um_path)
s4_5 <- read.csv(sample4_5um_path)
s5_5 <- read.csv(sample5_5um_path)
s6_5 <- read.csv(sample6_5um_path)
s7_5 <- read.csv(sample7_5um_path)

s1_20 <- read.csv(sample1_20um_path)
s2_20 <- read.csv(sample2_20um_path)
s3_20 <- read.csv(sample3_20um_path)

s1_60 <- read.csv(sample1_60um_path)
s2_60 <- read.csv(sample2_60um_path)
s3_60 <- read.csv(sample3_60um_path)
s4_60 <- read.csv(sample4_60um_path)

s1_200 <- read.csv(sample1_200um_path)
s2_200 <- read.csv(sample2_200um_path)
s3_200 <- read.csv(sample3_200um_path)
s4_200 <- read.csv(sample4_200um_path)

# Create vector of unique timestamps across all datasets sorted from least to greatest. 
# This step is necessary because some datasets are missing times

times <- sort(unique(c(s1_0$time, s2_0$time,
                       s1_5$time, s2_5$time, s3_5$time, s4_5$time, s5_5$time, s6_5$time, s7_5$time,
                       s1_20$time, s2_20$time, s3_20$time,
                       s1_60$time, s2_60$time, s3_60$time, s4_60$time, 
                       s1_200$time, s2_200$time, s3_200$time, s4_200$time)))

raw_data <- data.frame(time = times)

# Aligns data by timestaps 

raw_data <- left_join(raw_data, s1_0, by = "time") %>%
             left_join(., s2_0, by = "time") %>%
             left_join(., s1_5, by = "time") %>%
             left_join(., s2_5, by = "time") %>%
             left_join(., s3_5, by = "time") %>%
             left_join(., s4_5, by = "time") %>%
             left_join(., s5_5, by = "time") %>%
             left_join(., s6_5, by = "time") %>%
             left_join(., s7_5, by = "time") %>%
             left_join(., s1_20, by = "time") %>%
             left_join(., s2_20, by = "time") %>%
             left_join(., s3_20, by = "time") %>%
             left_join(., s1_60, by = "time") %>%
             left_join(., s2_60, by = "time") %>%
             left_join(., s3_60, by = "time") %>%
             left_join(., s4_60, by = "time") %>%
             left_join(., s1_200, by = "time") %>%
             left_join(., s2_200, by = "time") %>%
             left_join(., s3_200, by = "time") %>%
             left_join(., s4_200, by = "time")

names(raw_data) <- c("time", 
                      "s1_0", "s2_0", 
                      "s1_5", "s2_5", "s3_5", "s4_5", "s5_5", "s6_5", "s7_5", 
                      "s1_20", "s2_20", "s3_20",
                      "s1_60", "s2_60", "s3_60", "s4_60", 
                      "s1_200", "s2_200", "s3_200", "s4_200")

# Record speed instead of velocity
raw_data <- abs(raw_data)

px_per_cm_list <- list("s1_0" = 814,
                       "s2_0" = 1022,
                       "s1_5" = 355,
                       "s2_5" = 355,
                       "s3_5" = 1000,
                       "s4_5" = 1000, 
                       "s5_5" = 978,
                       "s6_5" = 978,
                       "s7_5" = 978,
                       "s1_20" = 383,
                       "s2_20" = 355,
                       "s3_20" = 918,
                       "s1_60" = 355,
                       "s2_60" = 355, 
                       "s3_60" = 1000,
                       "s4_60" = 1000,
                       "s1_200" = 355,
                       "s2_200" = 355,
                       "s3_200" = 1000,
                       "s4_200" = 1000)

# Convert pixel values in datasets to cm based on measured conversion factor

raw_data$s1_0 <- raw_data$s1_0 / px_per_cm_list[["s1_0"]]
raw_data$s2_0 <- raw_data$s2_0 / px_per_cm_list[["s2_0"]]

raw_data$s1_5 <- raw_data$s1_5 / px_per_cm_list[["s1_5"]]
raw_data$s2_5 <- raw_data$s2_5 / px_per_cm_list[["s2_5"]]
raw_data$s3_5 <- raw_data$s3_5 / px_per_cm_list[["s3_5"]]
raw_data$s4_5 <- raw_data$s4_5 / px_per_cm_list[["s4_5"]]
raw_data$s5_5 <- raw_data$s5_5 / px_per_cm_list[["s5_5"]]
raw_data$s6_5 <- raw_data$s6_5 / px_per_cm_list[["s6_5"]]
raw_data$s7_5 <- raw_data$s7_5 / px_per_cm_list[["s7_5"]]

raw_data$s1_20 <- raw_data$s1_20 / px_per_cm_list[["s1_20"]]
raw_data$s2_20 <- raw_data$s2_20 / px_per_cm_list[["s2_20"]]
raw_data$s3_20 <- raw_data$s3_20 / px_per_cm_list[["s3_20"]]

raw_data$s1_60 <- raw_data$s1_60 / px_per_cm_list[["s1_60"]]
raw_data$s2_60 <- raw_data$s2_60 / px_per_cm_list[["s2_60"]]
raw_data$s3_60 <- raw_data$s3_60 / px_per_cm_list[["s3_60"]]
raw_data$s4_60 <- raw_data$s4_60 / px_per_cm_list[["s4_60"]]

raw_data$s1_200 <- raw_data$s1_200 / px_per_cm_list[["s1_200"]]
raw_data$s2_200 <- raw_data$s2_200 / px_per_cm_list[["s2_200"]]
raw_data$s3_200 <- raw_data$s3_200 / px_per_cm_list[["s3_200"]]
raw_data$s4_200 <- raw_data$s4_200 / px_per_cm_list[["s4_200"]]

# Average the samples for each nicotine concentration into a single column

raw_data$conc_0um_mean <- rowMeans(raw_data[,c("s1_0", "s2_0")], na.rm = TRUE)
raw_data$conc_5um_mean <- rowMeans(raw_data[,c("s1_5", "s2_5", "s3_5", "s4_5", "s5_5", "s6_5", "s7_5")], na.rm = TRUE)
raw_data$conc_20um_mean <- rowMeans(raw_data[,c("s1_20", "s2_20", "s3_20")], na.rm = TRUE)
raw_data$conc_60um_mean <- rowMeans(raw_data[,c("s1_60", "s2_60", "s3_60", "s4_60")], na.rm = TRUE)
raw_data$conc_200um_mean <- rowMeans(raw_data[,c("s1_200", "s2_200", "s3_200", "s4_200")], na.rm = TRUE)

# Compress data from 30 obs/sec to 10 obs/sec by averaging data over 0.1s interval 

compressed_data <- data.frame(time = seq(min(raw_data$time), round(max(raw_data$time) - 1), by = 0.1))

compressed_data$s1_0 <- interval_mean(raw_data, "s1_0", 0.1)
compressed_data$s2_0 <- interval_mean(raw_data, "s2_0", 0.1)

compressed_data$s1_5 <- interval_mean(raw_data, "s1_5", 0.1)
compressed_data$s2_5 <- interval_mean(raw_data, "s2_5", 0.1)
compressed_data$s3_5 <- interval_mean(raw_data, "s3_5", 0.1)
compressed_data$s4_5 <- interval_mean(raw_data, "s4_5", 0.1)
compressed_data$s5_5 <- interval_mean(raw_data, "s5_5", 0.1)
compressed_data$s6_5 <- interval_mean(raw_data, "s6_5", 0.1)
compressed_data$s7_5 <- interval_mean(raw_data, "s7_5", 0.1)

compressed_data$s1_20 <- interval_mean(raw_data, "s1_20", 0.1)
compressed_data$s2_20 <- interval_mean(raw_data, "s2_20", 0.1)
compressed_data$s3_20 <- interval_mean(raw_data, "s3_20", 0.1)

compressed_data$s1_60 <- interval_mean(raw_data, "s1_60", 0.1)
compressed_data$s2_60 <- interval_mean(raw_data, "s2_60", 0.1)
compressed_data$s3_60 <- interval_mean(raw_data, "s3_60", 0.1)
compressed_data$s4_60 <- interval_mean(raw_data, "s4_60", 0.1)

compressed_data$s1_200 <- interval_mean(raw_data, "s1_200", 0.1)
compressed_data$s2_200 <- interval_mean(raw_data, "s2_200", 0.1)
compressed_data$s3_200 <- interval_mean(raw_data, "s3_200", 0.1)
compressed_data$s4_200 <- interval_mean(raw_data, "s4_200", 0.1)

compressed_data$conc_0um_mean <- rowMeans(compressed_data[,c("s1_0", "s2_0")], na.rm = TRUE)
compressed_data$conc_5um_mean <- rowMeans(compressed_data[,c("s1_5", "s2_5", "s3_5", "s4_5", "s5_5", "s6_5", "s7_5")], na.rm = TRUE)
compressed_data$conc_20um_mean <- rowMeans(compressed_data[,c("s1_20", "s2_20", "s3_20")], na.rm = TRUE)
compressed_data$conc_60um_mean <- rowMeans(compressed_data[,c("s1_60", "s2_60", "s3_60", "s4_60")], na.rm = TRUE)
compressed_data$conc_200um_mean <- rowMeans(compressed_data[,c("s1_200", "s2_200", "s3_200", "s4_200")], na.rm = TRUE)