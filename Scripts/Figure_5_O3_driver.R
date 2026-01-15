library(ggplot2)
library(tidyverse)
library(patchwork)

rm(list = ls())
gc()

# initialize
data_path <- "../Data"
data_path <- "/Users/liyangmingkai/Desktop/项目/Wildfire-Ozone/10_paper_plots/SA_V1/data_github/Data"

#################
# Figure 5A
#################
data <- readRDS(paste0(data_path, "/Smoke_O3_5types.rds")) %>%
  na.omit() %>%
  rename(
    `Base` = SO_base,
    `Ta` = SO_Ta,
    `UV` = SO_UV,
    `TaUV` = SO_TaUV,
    `ML` = SO_ML
         ) %>%
  pivot_longer(cols = c(`Base`, `Ta`, `UV`, `TaUV`, `ML`),
               names_to = "type",
               values_to = "Smoke O3") %>%
  mutate(type = factor(type, levels = c("Base", "Ta", "UV", "TaUV", "ML")))

data_sum <- data %>%
  group_by(lon, lat, type) %>%
  summarise(count = n(),
            `Smoke O3` = mean(`Smoke O3`), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(location = ifelse(lon < -100, "Western US", "Eastern US"))

data_boxplot <- data_sum %>%
  group_by(location, type) %>%
  summarize(
    cound = n(),
    lower = quantile(`Smoke O3`, 0.25), 
    upper = quantile(`Smoke O3`, 0.75),
    lower_max = quantile(`Smoke O3`, 0.1),
    upper_max = quantile(`Smoke O3`, 0.9), 
    Median = median(`Smoke O3`), 
    Mean = mean(`Smoke O3`)
  ) %>%
  ungroup()

# trim
data_filtered <- data_sum %>%
  group_by(type) %>%
  mutate(p01th = quantile(`Smoke O3`, 0.01),
         p99th = quantile(`Smoke O3`, 0.99)) %>%
  filter(`Smoke O3` >= p01th, `Smoke O3` <= p99th)

p1 <- ggplot() + 
  geom_violin(data = data_filtered, aes(x = type, y = `Smoke O3`, fill = location, col = location),
              trim = TRUE, 
              size = 1, 
              width = 0.8,  
              position = position_dodge(width = 0.8)) +  
  geom_boxplot(data = data_boxplot, 
               aes(x = type, fill = location, lower = lower, upper = upper, middle = Median, ymin = lower_max, ymax = upper_max), 
               stat = "identity",
               size = 1, 
               width = 0.3, 
               position = position_dodge(width = 0.8),  
               outlier.shape = NA) + 
  scale_fill_manual(values = c("Eastern US"= "#FEA600", "Western US" = "#B0E1E7")) +
  scale_color_manual(values = c("Eastern US"= "#FE8100", "Western US" = "#92C6E4")) +
  labs(
    x = NULL,
    y = "Smoke O3 [ppb]"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 13))

ggsave(filename = "Figure_5A.pdf", p1, width=6, height=4.5)

#################
# Figure 5B
#################
data <- readRDS(paste0(data_path, "/SO_ML_features.rds")) %>%
  rename(smoke_o3 = SO_ML)

# fire distance
data$distance_level <- NA
data$distance_level[data$fire_distance < 20] <- "<20"
data$distance_level[data$fire_distance >= 20 & data$fire_distance < 50] <- "20-50"
data$distance_level[data$fire_distance >= 50 & data$fire_distance < 100] <- "50-150"
data$distance_level[data$fire_distance >= 100 & data$fire_distance < 300] <- "150-300"
data$distance_level[data$fire_distance >= 300] <- ">300"

data$distance_level <- factor(data$distance_level, levels=c("<20", "20-50", "50-150", "150-300", '>300'))

summary_dist <- data %>%
  group_by(distance_level) %>%
  summarise(sum_SO = sum(smoke_o3),
            count_dist = n()) %>%
  ungroup() %>%
  mutate(SO_weight = sum_SO/sum(sum_SO)*100,
         count_dist = count_dist/sum(count_dist)*100)

# frp data
data$frp_level <- NA
data$frp_level[data$frp < 10] <- "<10"
data$frp_level[data$frp >= 10 & data$frp < 25] <- "10-25"
data$frp_level[data$frp >= 25 & data$frp < 50] <- "25-50"
data$frp_level[data$frp >= 50 & data$frp < 100] <- "50-100"
data$frp_level[data$frp >= 100] <- ">100"

data$frp_level <- factor(data$frp_level, levels=c("<10", "10-25", "25-50", "50-100", ">100"))

summary_frp <- data %>%
  group_by(frp_level) %>%
  summarise(sum_SO = sum(smoke_o3),
            count_frp = n()) %>%
  ungroup() %>%
  mutate(SO_weight = sum_SO/sum(sum_SO)*100,
         count_frp = count_frp/sum(count_frp)*100)

# no2 and hcho
threshold <- 0.5
data$type <- "No increase"
data$type[data$no2 > data$no2_base+data$no2_sd*threshold & !(data$hcho > data$hcho_base+data$hcho_sd*threshold)] <- "+NO2 only"
data$type[data$hcho > data$hcho_base+data$hcho_sd*threshold & !(data$no2 > data$no2_base+data$no2_sd*threshold)] <- "+HCHO only"
data$type[data$hcho > data$hcho_base+data$hcho_sd*threshold & data$no2 > data$no2_base+data$no2_sd*threshold] <- "+NO2 & +HCHO"

data$type_level <- factor(data$type, levels=c("No increase", "+NO2 only", "+HCHO only", "+NO2 & +HCHO"))

summary_regime <- data %>%
  group_by(type_level) %>%
  summarise(sum_SO = sum(smoke_o3),
            count_regime = n()) %>%
  ungroup() %>%
  mutate(SO_weight = sum_SO/sum(sum_SO)*100,
         count_regime = count_regime/sum(count_regime)*100)

# visualize
summary_dist$group <- "Fire distance"
summary_frp$group <- "FRP"
summary_regime$group <- "NO2 & HCHO"

# unify
colnames(summary_dist)[1] <- "level"
colnames(summary_frp)[1] <- "level"
colnames(summary_regime)[1] <- "level"

colnames(summary_dist)[3] <- "sample_count"
colnames(summary_frp)[3] <- "sample_count"
colnames(summary_regime)[3] <- "sample_count"

# bind all types
all_summary <- bind_rows(summary_dist, summary_frp, summary_regime) %>%
  mutate(group = factor(group, levels = c("NO2 & HCHO", "Fire distance", "FRP")),
         level = factor(level, levels = unique(level)))

# plot Figure 5B
ggplot(all_summary, aes(x = group, y = SO_weight, fill = level)) +
  geom_bar(stat = "identity", position = "stack", col="black", width=0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    # Fire distance colors
    "#C7EAE5", "#80CDC1", "#35978F", "#01665E", "#003C30",
    # FRP colors
    "#E7D4E8", "#C2A5CF", "#996FB0", "#762A83", "#40003C",
    # Regime colors
    "#DFC27D", "#BF812D", "#8C510A", "#543005"
  )) +
  labs(
    x = NULL, y = "Relative contribution (%)",
    fill = "Category",
  ) +
  theme_test() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0,100,25), labels = seq(0,100,25), limits = c(0,100.1), expand = c(0,0))

ggsave(filename = "Figure 5B.pdf", width=6, height=4)
