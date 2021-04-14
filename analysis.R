library(tidyverse)

# File paths

sample1_5um_path <- "03:25:2021/5uM_1/Results/vid1_TS_W1558_speed.csv"
sample2_5um_path <- "03:25:2021/5uM_2/Results/5um_sample2_modified_TS_W520_speed.csv"

sample1_20um_path <- "03:25:2021/20uM_1/Results/20um_sample1_modified_TS_W591_speed.csv"
sample2_20um_path <- "03:25:2021/20uM_2/Results/20um_sample2_modified_TS_W1681_speed.csv"

sample1_60um_path <- "03:25:2021/60uM_1/Results/60um_sample1_modified_TS_W1226_speed.csv"
sample2_60um_path <- "03:25:2021/60uM_2/Results/60um_sample2_modified_TS_W2090_speed.csv"

sample1_200um_path <- "03:25:2021/200uM_1/Results/200um_sample1_modified_TS_W2834_speed.csv"
sample2_200um_path <- "03:25:2021/200uM_2/Results/200um_sample2_modified_TS_W2393_speed.csv"

# Load .csv data

s1_5 <- read.csv(sample1_5um_path)
s2_5 <- read.csv(sample2_5um_path)

s1_20 <- read.csv(sample1_20um_path)
s2_20 <- read.csv(sample2_20um_path)

s1_60 <- read.csv(sample1_60um_path)
s2_60 <- read.csv(sample2_60um_path)

s1_200 <- read.csv(sample1_200um_path)
s2_200 <- read.csv(sample2_200um_path)

# Create vector of unique timestamps across all datasets sorted from least to greatest. 
# This step is necessary because some datasets are missing times

times <- sort(unique(c(s1_5$time, s2_5$time, s1_20$time, s2_20$time, s1_60$time, s2_60$time, s1_200$time, s2_200$time)))

master_df <- data.frame(time = times)

# Aligns speeds by timestaps 

master_df <- left_join(master_df, s1_5, by = "time") %>%
             left_join(., s2_5, by = "time") %>%
             left_join(., s1_20, by = "time") %>%
             left_join(., s2_20, by = "time") %>%
             left_join(., s1_60, by = "time") %>%
             left_join(., s2_60, by = "time") %>%
             left_join(., s1_200, by = "time") %>%
             left_join(., s2_200, by = "time")

# Convert negative values to positive values
master_df <- abs(master_df)

names(master_df) <- c("time", "s1_5", "s2_5", "s1_20", "s2_20", "s1_60", "s2_60", "s1_200", "s2_200")

#ggplot(data = master_df, aes(x = time)) + geom_point(aes(y = s1_5, colour = "5 uM")) + geom_point(aes(y = s2_5, colour = "5 uM")) + geom_point(aes(y = s1_20, colour = "20 uM")) + geom_point(aes(y = s2_20, colour = "20 uM")) + geom_point(aes(y = s1_60, colour = "60 uM")) + geom_point(aes(y = s2_60, colour = "60 uM")) + geom_point(aes(y = s1_200, colour = "200 uM")) + geom_point(aes(y = s2_200, colour = "200 uM"))

master_df[is.na(master_df)] <- 0

px_per_cm <- 355

# Average the two samples for each nicotine concentration into a new column

master_df$conc_5um_mean <- rowMeans(master_df[,c("s1_5", "s2_5")]) / px_per_cm
master_df$conc_20um_mean <- rowMeans(master_df[,c("s1_20", "s2_20")]) / px_per_cm
master_df$conc_60um_mean <- rowMeans(master_df[,c("s1_60", "s2_60")]) / px_per_cm
master_df$conc_200um_mean <- rowMeans(master_df[,c("s1_200", "s2_200")]) / px_per_cm

#conc_5um_model_p3 <- lm(conc_5um_mean ~ poly(time, 3), data = master_df)
#conc_20um_model_p3 <- lm(conc_20um_mean ~ poly(time, 3), data = master_df)
#conc_60um_model_p3 <- lm(conc_60um_mean ~ poly(time, 3), data = master_df)
#conc_200um_model_p3 <- lm(conc_200um_mean ~ poly(time, 3), data = master_df)

#conc_5um_model_power <- nls(conc_5um_mean ~ a*time^b+c, start=list(a = 1, b = 1, c = 1), data = master_df)
#conc_20um_model_power <- nls(conc_20um_mean ~ a*time^b+c, start=list(a = 1, b = 1, c = 1), data = master_df)
#conc_60um_model_power <- nls(conc_60um_mean ~ a*time^b+c, start=list(a = 1, b = 1, c = 1), data = master_df)
#conc_200um_model_power <- nls(conc_200um_mean ~ a*time^b+c, start=list(a = 1, b = 1, c = 1), data = master_df)

ggplot(data = master_df) + geom_smooth(aes(x = time, y = conc_5um_mean), method = 'loess', color = 'red') + geom_smooth(aes(x = time, y = conc_20um_mean), method = 'loess', color = "blue") + geom_smooth(aes(x = time, y = conc_60um_mean), method = 'loess', color = "purple") + geom_smooth(aes(x = time, y = conc_200um_mean), method = 'loess', color = "green") + labs(x = "Time (seconds)", y = "Velocity (cm/s)", title = "Locomotion Assay") + theme_minimal(base_size = 20)

