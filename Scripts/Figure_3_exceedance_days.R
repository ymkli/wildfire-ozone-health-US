library(ggplot2)
library(tidyverse)
library(patchwork)

rm(list = ls())
gc()

# initialize
data_path <- "../Data"

# load data
data_base <- readRDS(paste0(data_path, "/EPA_station_TaUV_SO.rds")) %>%
  select(lon, lat, date, baseline_o3)

data <- readRDS(paste0(data_path, "/EPA_Station_2006_2023_daily_O3.rds")) %>%
  left_join(data_base) %>%
  mutate(
    baseline_o3 = ifelse(is.na(baseline_o3), 0, baseline_o3),
    year = year(date),
    month = month(date),
    location = ifelse(lon < -100, "Western US", "Eastern US"),
    location = factor(location, levels = c("Western US", "Eastern US"))
    ) %>%

  group_by(location, year) %>%
  summarise(
    day_num = n(),
    smoke_exceedance = sum(smokePM_pred > 0 & o3_mda8_ppb > 70 & baseline_o3 < 70),
    nonsmoke_exceedance = sum(smokePM_pred <= 0 & o3_mda8_ppb > 70) + sum(smokePM_pred > 0 & o3_mda8_ppb > 70 & baseline_o3 > 70),
    smoke_driven = smoke_exceedance/day_num*100,
    nonsmoke_driven = nonsmoke_exceedance/day_num*100
    ) %>%
  ungroup() %>%
  
  pivot_longer(
    cols = c(smoke_driven, nonsmoke_driven),
    names_to = "type",
    values_to = "rate"
  ) %>%
  mutate(type = factor(type, levels = c("smoke_driven", "nonsmoke_driven")))


ggplot(data, aes(x = year, y = rate, fill = type)) +
  geom_area(alpha = 1, color = "white", size = 0.2) +
  facet_wrap(~location, scales = "free_y") +
  scale_fill_manual(
    values = c("smoke_driven" = "#FC9272",
               "nonsmoke_driven" = "#66C2A4"),
  ) +
  scale_x_continuous(
    limits = c(2006, 2023),
    breaks = seq(2006, 2023, 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 7),
    breaks = seq(0, 6, 2),
    expand = c(0, 0)
  ) +
  labs(x = "",
       y = "O3 exceedance days [%]",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 14),
    panel.spacing = unit(1, "lines")
  )

ggsave(filename = "Figure_3.pdf", width=9, height=4.5)
