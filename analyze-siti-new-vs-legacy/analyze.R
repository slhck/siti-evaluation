library(tidyverse)
library(tidyjson)
library(ggpointdensity)
library(tidylog)

# ==========================================================================================

read_plus <- function(flnm, ...) {
  read_csv(flnm, ...) %>% 
    mutate(filename = flnm)
}

# ==========================================================================================

df.scene_cuts = list.files("data", pattern = "scene_cuts.csv", full.names = T) %>% 
  map_df(~read_plus(., col_names = "n")) %>% 
  mutate(across(filename, basename)) %>% 
  mutate_at("filename", str_remove, "_SDR-scene_cuts.csv") %>% 
  relocate(filename, .before = n) %>% 
  rename(video = filename) %>% 
  mutate_at("video", str_extract, "([a-zA-Z]+)")

df.siti_legacy = list.files("data", pattern = "siti_legacy.csv", full.names = T) %>% 
  map_df(~read_plus(.)) %>% 
  mutate(across(filename, basename)) %>% 
  select(-input_file) %>% 
  mutate_at("filename", str_remove, "-siti_legacy.csv") %>% 
  relocate(filename) %>% 
  mutate(
    type = if_else(str_detect(filename, "SDR"), "SDR", "HDR10")
  ) %>% 
  mutate_at("filename", str_remove, "_SDR|_HDR10") %>% 
  mutate(variant = "legacy") %>% 
  rename(video = filename)

df.siti_new = list.files("data", pattern = "siti.csv", full.names = T) %>% 
  map_df(~read_plus(.)) %>% 
  mutate(across(filename, basename)) %>% 
  select(-input_file) %>% 
  mutate_at("filename", str_remove, "-siti.csv") %>% 
  relocate(filename) %>% 
  mutate(
    type = if_else(str_detect(filename, "SDR"), "SDR", "HDR10")
  ) %>% 
  mutate_at("filename", str_remove, "_SDR|_HDR10") %>% 
  mutate(variant = "new") %>% 
  rename(video = filename)

df.yavg = list.files("data", pattern = "YAVG.csv", full.names = T) %>% 
  map_df(~read_plus(., col_names = c("yavg"), col_select = c("yavg"))) %>% 
  mutate(across(filename, basename)) %>% 
  mutate_at("filename", str_remove, "-YAVG.csv") %>% 
  relocate(filename) %>% 
  rename(video = filename) %>% 
  mutate_at("video", str_extract, "([a-zA-Z]+)") %>% 
  group_by(video) %>% 
  mutate(n = row_number())

# ==========================================================================================

df.siti_legacy %>% 
  count(video, type)

df.siti_new %>% 
  count(video, type)

# merge
df.siti =
  df.siti_legacy %>% 
  left_join(df.siti_new, suffix = c(".legacy", ".new"), by = c("video", "type", "n")) %>% 
  mutate_at("video", str_extract, "([a-zA-Z]+)") %>% 
  left_join(df.yavg)

# ==========================================================================================

# old vs new scores
df.siti %>% ggplot(aes(x = si.legacy, y = si.new)) + 
  geom_pointdensity(adjust = 10) + 
  scale_color_viridis_c() +
  facet_grid(video~type) +
  theme(legend.position = "none")
ggsave("plots/legacy_vs_new.pdf")

# over time
df.siti %>% 
  filter(type == "SDR") %>% 
  select(-starts_with("ti")) %>% 
  pivot_longer(c(si.new, si.legacy), names_to = "variant") %>% 
  ggplot(aes(x = n, y = value, color = variant)) +
  geom_line() +
  facet_grid(video~.)
ggsave("plots/sdr_legacy_vs_new_over_time.pdf")

# over time, scaled
df.siti %>% 
  filter(type == "SDR") %>% 
  select(-starts_with("ti")) %>% 
  group_by(video) %>% 
  mutate(across(starts_with("si."), scale)) %>% 
  ungroup() %>% 
  pivot_longer(c(si.new, si.legacy), names_to = "variant") %>% 
  ggplot(aes(x = n, y = value, color = variant)) +
  geom_line() +
  facet_grid(video~.)
ggsave("plots/sdr_legacy_vs_new_over_time_scaled.pdf")

# compare SDR and HDR variants
df.siti %>% 
  ggplot(aes(x = si.new, fill = type)) +
  geom_density(alpha = 0.3) +
  facet_grid(video~.)
ggsave("plots/sdr_vs_hdr_density.pdf")

df.siti %>% 
  select(-starts_with("ti")) %>% 
  ggplot(aes(x = n, y = si.new, color = type)) +
  geom_line() +
  facet_grid(video~.)
ggsave("plots/sdr_vs_hdr_over_time.pdf")

# spread between old and new
# df.siti %>% 
#   filter(type == "SDR") %>% 
#   mutate(spread = abs(si.legacy - si.new)) %>% 
#   ggplot(aes(x = n, y = spread)) +
#   geom_line() +
#   facet_grid(video~.)

# does it depend on luminance?
df.siti %>% 
  filter(type == "SDR") %>% 
  mutate(spread = abs(si.legacy - si.new)) %>% 
  ggplot(aes(x = yavg, y = spread)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  facet_grid(video~.)
ggsave("plots/spread_vs_luminance_corr.pdf")

# over time
df.siti %>% 
  filter(type == "SDR") %>% 
  mutate(spread = abs(si.legacy - si.new)) %>% 
  group_by(video) %>% 
  mutate(across(c(spread, yavg), scale)) %>% 
  ungroup() %>% 
  pivot_longer(c(spread, yavg)) %>% 
  ggplot(aes(x = n, y = value, color = name)) +
  geom_line() +
  facet_grid(video~.)
ggsave("plots/spread_vs_luminance.pdf")
