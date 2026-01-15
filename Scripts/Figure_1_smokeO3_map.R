library(ggplot2)
library(tidyverse)
library(maps)
library(patchwork)
library(sf)

rm(list = ls())
gc()

# initialize
usa_map <- map_data("state")

col1 <- c("#4575B4FF", "#74ADD1FF", "#ABD9E9FF", "#E0F3F8FF", "#FEE090FF", "#FDAE61FF", "#D73027FF")
col2<- c("#CCFFFF","#6EB6F1", "#699E1D", "#fff179","#EA8A39","#A3340D","#470F27")
col3 <- c("#000003", "#200C4A", "#570F6D", "#892269", "#BB3755", "#E35932", "#F98D09", "#F8CB34", "#FCFEA4")

data_path <- "../Data"

# load data
data <- readRDS(paste0(data_path, "/EPA_station_TaUV_SO.rds"))

data_station <- data %>%
  filter((!is.na(baseline_o3)) & o3_mda8_ppb != 0) %>% # filter NA
  mutate(smoke_o3 = o3_mda8_ppb - baseline_o3) %>%
  
  # station-level average
  group_by(id, lon, lat, count) %>%
  summarise(
    o3_mda8_ppb = mean(o3_mda8_ppb, na.rm = TRUE),
    baseline_o3 = mean(baseline_o3, na.rm = TRUE),
    smoke_o3 = mean(smoke_o3, na.rm = TRUE),
    .groups = "drop"
    ) %>% 
  mutate(smoke_o3_df = smoke_o3/baseline_o3*100) %>%
  filter(count > 20) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

#######################################
# Figure 1A: O3 during smoke day
#######################################
p1 <- ggplot()+
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "white", color = "#AAAAAA")+
  geom_sf(data = data_station, aes(col = o3_mda8_ppb), size = 0.5) +
  scale_color_gradientn(
    colors = col1, 
    na.value = NA,
    name = "O3 [ppb]",
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      barwidth = 10,
      barheight = 0.75,
      ticks.colour = "white",
      ticks.linewidth = 0.2
    ),
    limits = c(30, 65),
    oob = scales::squish
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        text = element_text(size = 8))

ggsave(p1, filename = "Figure_1A.pdf", width=4, height=3)

#######################################
# Figure 1B: O3 during non-smoke day
#######################################
p2 <- ggplot()+
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "white", color = "#AAAAAA")+
  geom_sf(data = data_station, aes(col = baseline_o3), size = 0.5) +
  scale_color_gradientn(
    colors = col1, 
    na.value = NA,
    name = "O3 [ppb]",
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      barwidth = 10,
      barheight = 0.75,
      ticks.colour = "white",
      ticks.linewidth = 0.2
    ),
    limits = c(30, 65),
    oob = scales::squish
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        text = element_text(size = 8))

ggsave(p2, filename = "Figure_1B.pdf", width=4, height=3)

#######################################
# Figure 1C: smoke O3
#######################################
p3 <- ggplot()+
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "white", color = "#AAAAAA")+
  geom_sf(data = data_station, aes(col = smoke_o3), size = 0.5) +
  scale_color_gradientn(
    colors = col2,
    na.value = NA,
    name = "O3 [ppb]",
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      barwidth = 10,
      barheight = 0.75,
      ticks.colour = "white",
      ticks.linewidth = 0.2
    ),
    limits = c(0, 8),
    oob = scales::squish
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        text = element_text(size = 8))

ggsave(p3, filename = "Figure_1C.pdf", width=4, height=3)

#######################################
# Figure 1D: smoke O3 (%)
#######################################
p4 <- ggplot()+
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "white", color = "#AAAAAA")+
  geom_sf(data = data_station, aes(col = smoke_o3_df), size = 0.5) +
  scale_color_gradientn(
    colors = rev(col3),
    na.value = NA,
    name = "O3 [ppb]",
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      barwidth = 10,
      barheight = 0.75,
      ticks.colour = "white",
      ticks.linewidth = 0.2
    ),
    limits = c(0, 15),
    oob = scales::squish
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        text = element_text(size = 8))

ggsave(p4, filename = "Figure_1D.pdf", width=4, height=3)

