library(tidyverse)
library(tidyjson)
library(ggpointdensity)
library(tidylog)
library(ggrepel)
library(cowplot)

# =============================================================================
# LOAD DATA

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

data = Sys.glob("data/*.csv") %>% 
  map_df(~read_plus(.)) %>% 
  select(-input_file) %>% 
  mutate_at(vars(filename), tools::file_path_sans_ext) %>% 
  mutate_at(vars(filename), basename) %>% 
  separate(filename, into = c("src", "bitrate", "resolution", "fps", "codec"), sep = "-") %>% 
  relocate(where(is_character)) %>% 
  mutate_at(vars(bitrate, resolution, fps), as.double) %>% 
  filter(!src %in% c("american_football_harmonic", "cutting_orange_tuil", "water_netflix"))

# =============================================================================
# PLOTS

data %>% 
  group_by(src) %>% 
  # retain highest bitrate onnly
  group_by(src, resolution) %>% 
  filter(bitrate == max(resolution, bitrate)) %>% 
  ungroup() %>% 
  mutate(avg_si = mean(si)) %>%
  ggplot(aes(x = factor(resolution), y = si)) +
  geom_boxplot(outlier.size = .5) +
  facet_grid(src~.) +
  coord_flip() +
  theme(strip.text.y = element_text(angle = 0))
ggsave("plots/si_range-histogram.pdf")

data %>% 
  # retain highest bitrate onnly
  group_by(src, resolution) %>% 
  filter(bitrate == max(resolution, bitrate)) %>% 
  ungroup() %>% 
  group_by(src, resolution) %>% 
  summarize(
    min_si = min(si),
    max_si = max(si),
    si_range = max(si) - min(si)
  ) %>% 
  group_by(src) %>% 
  summarize(si_range = max(max_si) - min(min_si)) %>% 
  left_join(
    data %>% 
      filter(resolution == 1080) %>% 
      group_by(src) %>% 
      summarize(
        avg_si = mean(si),
        median_si = median(si)
      )
  ) %>% 
  ggplot(aes(x = avg_si, y = si_range, label = src)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label_repel()
ggsave("plots/avg_si-si_range.pdf")

data %>% 
  group_by(src, resolution) %>% 
  filter(bitrate == max(resolution, bitrate)) %>% 
  ungroup() %>% 
  mutate(avg_si = mean(si)) %>% 
  ggplot(aes(x = n, y = si, color = factor(resolution))) +
  geom_line() +
  scale_color_viridis_d(end = 0.7) +
  facet_grid(reorder(src, avg_si)~.) +
  theme(
    strip.text.y = element_text(angle = 0),
    legend.position = "bottom"
  )
ggsave("plots/si_range.pdf")
