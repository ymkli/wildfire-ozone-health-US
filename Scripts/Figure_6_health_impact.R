library(data.table)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(readxl)
library(sf)
library(patchwork)

rm(list = ls())
gc()

# initialize
col1<- c("#CCFFFF","#6EB6F1", "#699E1D", "#fff179","#EA8A39","#A3340D","#470F27")

data_path <- "../Data"

# load data

## population (state level)
pop_data <- readRDS(paste0(data_path, "/Population_by_state.rds"))

## mortality by year
data_byyear <- readxl::read_excel(paste0(data_path, "/Mortality_SO3_SPM_byyear.xlsx"))

## mortality by state (smoke O3)
data_bystate <- readRDS(paste0(data_path, "/Mortality_SO3_by_state.rds"))

## mortality by state (smoke PM2.5)
data_bystate_PM <- readRDS(paste0(data_path, "/Mortality_SPM_by_state.rds"))

## mortality due to total ozone
totalo3_data <- readxl::read_excel(paste0(data_path, "/Mortality_totalO3_byyear.xlsx"))

## mortality in different SPM bin
data_bin <- readRDS(paste0(data_path, "/Mortality_SPMbin.rds"))

##############################################
# panel(1): smoke O3 vs total O3 trend
##############################################
data_f1 <- data_byyear %>%
  filter(study == "Sun et al. 2022") %>%
  select(c(year, death_mean)) %>%
  left_join(totalo3_data)

ggplot(data_f1, aes(x=year)) +
  geom_col(aes(y = death_totalO3), fill = "#66C2A4", col = "#1B6F47", position = "dodge", width = 0.7) +
  geom_col(aes(y = death_mean), fill = "#FC9272", col = "#B93B2E", position = "dodge", width = 0.7) +
  labs(
    x = "", y = "Deaths from O3",
    fill = "", col = ""
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(limits=c(2005.5, 2023.5), breaks=seq(2006, 2023, 4), expand=c(0, 0))+
  scale_y_continuous(limits=c(0, 46000), breaks=seq(0, 40000, 10000), expand=c(0, 0))+
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

ggsave(filename = "Figure_6A.pdf", width=4.5, height=10/3)


##############################################
# panel(2): smoke O3 on each state
##############################################

data_f2 <- left_join(data_bystate, pop_data) %>%
  mutate(death_rate_mean = death_mean_o3/pop*1000000,
         death_rate_mean = ifelse(death_rate_mean <= 0, 1, death_rate_mean))

p2 <- ggplot(data_f2, aes(geometry = geometry, fill = death_rate_mean)) +
  geom_sf() +
  theme_void() +
  scale_fill_gradientn(
    colours = col1,
    trans = "sqrt",
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
  labs(fill = "Deaths per million due to smoke O3") +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5),)

ggsave(filename = "Figure_6B.pdf", p2, width=4.5, height=10/3)


##############################################
# panel(3) state PM vs O3
##############################################

data_f3 <- data_bystate %>%
  left_join(data_bystate_PM) %>%
  filter(rank(-death_mean_o3) <= 17 | rank(-death_mean_pm) <= 16)

ggplot(data_f3, aes(x = death_mean_pm, y = death_mean_o3)) +
  geom_point(color = "#FC9272", size = 3) +
  geom_text_repel(aes(label = STUSPS), size = 4, max.overlaps = Inf) +
  labs(
    x = "Annual death due to Smoke PM2.5",
    y = "Annual death due to Smoke O3",
  ) +
  theme_classic()

ggsave(filename = "Figure_6C.pdf", width=5.8, height=4.5)

##############################################
# panel(4): death during small PM 
##############################################

ggplot(data_bin, aes(x = interval, y = study, fill = mean_diff_p)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(mean_diff_p, 1), "%"),
                color = mean_diff_p > 15), 
            size = 3.5, fontface = "bold") +
  scale_fill_gradientn(
    colours = c("#f7fbff", "#6baed6", "#08306b"), 
    name = "Contribution to smoke related death (%)",
    guide = guide_colorbar(
      title.position = "top",      
      title.hjust = 0.5           
    )
  ) +
  labs(x="Smoke PM2.5 interval", y="") +
  scale_color_manual(
    values = c("TRUE" = "white", "FALSE" = "black"),
    guide = "none"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(color="black"),
    legend.position = "bottom",                     
    legend.key.width = unit(1.8, "cm"),             
    legend.key.height = unit(0.5, "cm"),
    legend.title = element_text(size=12, face="bold")  
  )

ggsave(filename = "Figure_6D.pdf", width=6.75, height=5)

##############################################
# panel(5): smoke O3 vs smoke PM
##############################################
data_f5 <- data_byyear %>%
  filter(study == "Sun et al. 2022") %>%
  select(c(year, death_mean, death_PM_mean)) %>%
  rename(`Smoke O3` = death_mean,
          `Smoke PM2.5` = death_PM_mean) %>%
  pivot_longer(cols = c(`Smoke O3`, `Smoke PM2.5`),
               names_to = "study",
               values_to = "death_mean")

ggplot(data_f5, aes(x=year, y=death_mean, group=study, fill=study, col=study)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  labs(
    x = "Year", y = "Annual deaths",
    fill = "", col = ""
  ) +
  scale_fill_manual(values = c("Smoke PM2.5" = "#F2F2F2", "Smoke O3" = "#FC9272")) +
  scale_color_manual(values = c("Smoke PM2.5" = "#BFBFBF", "Smoke O3" = "#B93B2E")) +
  scale_x_continuous(limits=c(2005.5, 2023.5),breaks=seq(2006, 2023, 4),expand=c(0, 0))+
  scale_y_continuous(limits=c(0, 21000),breaks=seq(0, 21000, 5000),expand=c(0, 0))+
  theme_classic() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

ggsave(filename = "Figure_6E.pdf", width=9, height=10/3)

