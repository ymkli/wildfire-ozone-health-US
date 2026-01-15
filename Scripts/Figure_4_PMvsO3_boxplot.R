library(ggplot2)
library(tidyverse)
library(dplyr)
library(broom)
library(purrr)
library(patchwork)

rm(list = ls())
gc()

# initialize
data_path <- "../Data"

# load data
data <- readRDS(paste0(data_path, "/EPA_station_TaUV_SO.rds")) %>%
  filter(!is.na(baseline_o3) & o3_mda8_ppb != 0) %>%
  mutate(smoke_ozone = o3_mda8_ppb - baseline_o3,
         location = if_else(lon >= -100, "Eastern U.S", "Western U.S."))

# SPM bin
data$PM_level <- "0-5"
data$PM_level[data$smokePM_pred>5 & data$smokePM_pred<=15] <- "5-15"
data$PM_level[data$smokePM_pred>15 & data$smokePM_pred<=25] <- "15-25"
data$PM_level[data$smokePM_pred>25 & data$smokePM_pred<=50] <- "25-50"
data$PM_level[data$smokePM_pred>50 & data$smokePM_pred<=75] <- "50-75"
data$PM_level[data$smokePM_pred>75 & data$smokePM_pred<=100] <- "75-100"
data$PM_level[data$smokePM_pred>100 & data$smokePM_pred<=150] <- "100-150"
data$PM_level[data$smokePM_pred>150 ] <- ">150"

data$PM_level <- factor(data$PM_level, levels = c("0-5", "5-15", "15-25", "25-50", "50-75", "75-100", "100-150", ">150"))

data_count <- data %>%
  group_by(PM_level, location) %>%
  summarise(count = n(),
            correlation = cor(smoke_ozone, smokePM_pred)) 

data_t_test <- data %>%
  group_by(PM_level) %>%
  do({
    # PM_level bin
    east <- filter(., location == "Eastern U.S")$smoke_ozone
    west <- filter(., location == "Western U.S.")$smoke_ozone
    
    # t-test
    t_test_result <- t.test(east, west, alternative = "greater")
    
    # p-value
    data.frame(PM_level = unique(.$PM_level), p_value = t_test_result$p.value)
  }) %>%
  ungroup()

data_boxplot <- data %>%
  group_by(location, PM_level) %>%
  summarize(
    cound = n(),
    lower = quantile(smoke_ozone, 0.25), # 25th
    upper = quantile(smoke_ozone, 0.75), # 75th
    lower_max = quantile(smoke_ozone, 0.1), # 10th
    upper_max = quantile(smoke_ozone, 0.9), # 90th
    Median = median(smoke_ozone), 
    Mean = mean(smoke_ozone)
  ) %>%
  ungroup()

ggplot(data_boxplot, aes(x = PM_level, fill = location)) +
  geom_boxplot(aes(lower = lower, upper = upper, middle = Median, ymin = lower_max, ymax = upper_max), 
               stat = "identity",
               size = 1, 
               width = 0.5, 
               position = position_dodge(width = 0.6),
               outlier.shape = NA) + 
  geom_point(aes(y = Mean, group = location),
             col = "red", 
             shape = 18, 
             size = 4, 
             position = position_dodge(width = 0.6)) +
  scale_fill_manual(values = c("#F1AA3C", "#BAE0E6")) +
  theme_bw()+
  labs(x = "Smoke PM2.5", y = "Smoke Ozone") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom"
  )

ggsave(filename = "Figure_4.pdf", width=9, height=4.5)
