library(ggplot2)
library(tidyverse)
library(maps)
library(patchwork)
library(sf)

rm(list = ls())
gc()

# initialize
col1<- c("#CCFFFF","#6EB6F1", "#699E1D", "#fff179","#EA8A39","#A3340D","#470F27")

usa_map <- map_data("state")

data_path <- "../Data"

# load data
smoke_freq <- readRDS(paste0(data_path, "/EPA_station_SmokeFreq.rds"))
data_station <- readRDS(paste0(data_path, "/EPA_station_TaUV_SO.rds"))

data <- data_station %>%
  filter((!is.na(baseline_o3)) & o3_mda8_ppb != 0) %>%
  mutate(
    smoke_o3 = o3_mda8_ppb - baseline_o3,
    year = year(date)
    ) %>%
  
  # station-level yearly avg
  group_by(grid_id_10km, lon, lat, year) %>%
  summarise(
    smoke_o3 = mean(smoke_o3, na.rm = TRUE),
    .groups = "drop"
    ) %>%
  
  # all-day avg by year
  left_join(smoke_freq) %>%
  mutate(
    day_num = ifelse(year%%4 == 0, 366, 365),
    smoke_o3_allday = smoke_o3*fire_time/day_num
    ) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# valid for sqrt
data$smoke_o3_allday <- ifelse(data$smoke_o3_allday > 0, data$smoke_o3_allday, 1e-8)

ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "white", color = "#AAAAAA") + 
  geom_sf(data = data, aes(col = smoke_o3_allday), size = 0.5) +
  facet_wrap(~ year, ncol = 5) + 
  scale_color_gradientn(
    colors = col1, 
    na.value = NA,
    oob = scales::squish,
    limits = c(0, 5),
    trans = "sqrt",
    name = "Annual average O3 [ppb/m3]",
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      barwidth = 10,
      barheight = 0.75,
      ticks.colour = "white",
      ticks.linewidth = 0.2
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    text = element_text(size = 8)
  )

ggsave(filename = "Figure_2.pdf", width=15, height=10)

