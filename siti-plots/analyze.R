library(tidyverse)
library(ggplot2)
library(slider)
library(ggrepel)
library(cowplot)

theme_set(theme_bw() + theme(text=element_text(size=16,  family="Times")))

df.siti_avt = list.files("data/siti", pattern = "csv", full.names = TRUE) %>% 
  map_df(read_csv) %>% 
  rename(src = input_file, frame_no = n) %>% 
  mutate_at("src", tools::file_path_sans_ext) %>% 
  filter(!str_detect(src, "8s")) %>% 
  mutate_at(vars(src), str_replace, "LeagueOfLegends", "League_of_Legends") %>% 
  mutate(dataset = "avt-vqdb-uhd1")

df.siti_vqeg = list.files("data", pattern = "csv", full.names = TRUE) %>% 
  map_df(read_csv) %>% 
  rename(src = input_file, frame_no = n) %>% 
  mutate_at("src", tools::file_path_sans_ext) %>% 
  mutate_at(vars(src), str_replace, "vqeghd1_", "") %>% 
  mutate_at(vars(src), str_replace, "_original-420p", "") %>% 
  mutate(dataset = "vqeghd1") %>% 
  mutate(ti = if_else(ti == 0, NA_real_, ti))

df.siti = bind_rows(df.siti_avt, df.siti_vqeg)

df.siti %>% 
  group_by(src) %>% 
  mutate(
    ti_mov_avg = slider::slide_dbl(ti, mean, na.rm = T, .before = 120, .after = 0),
    ti_diff = abs(ti - ti_mov_avg)
  ) %>% 
  ggplot(aes(x = src, y = ti_diff)) +
  geom_boxplot() +
  facet_grid(~dataset, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/scene_cuts.pdf")

df.siti %>% 
  group_by(src) %>% 
  filter(frame_no != 1) %>% 
  group_by(src) %>% 
  # remove scene cuts
  filter(ti < quantile(ti, 0.99, na.rm = T)) %>% 
  ungroup() %>%
  group_by(dataset) %>% 
  group_map(~ {
    .x %>% ggplot(aes(x = frame_no, y = ti)) +
    geom_line() +
    facet_grid(src~., scales = "free") +
    theme(strip.text.y = element_text(angle = 0))
  }) %>% 
  plot_grid(plotlist = ., nrow = 2, ncol = 1)
ggsave("plots/line_plot.pdf", height = 12, width = 16)

df.siti_summarized = 
  df.siti %>%
  mutate_at(
    vars(src), str_replace_all,
    c(
      "american_football_harmonic" = "AFH",
      "bigbuck_bunny_8bit" = "BBB",
      "cutting_orange_tuil" = "CO",
      "Sparks_cut_13" = "SC13",
      "Sparks_cut_15" = "SC15",
      "surfing_sony_8bit" = "SS",
      "vegetables_tuil" = "VT",
      "water_netflix" = "WN"
    )
  ) %>%
  # remove scene cuts
  group_by(src) %>% 
  filter(ti < quantile(ti, 0.99, na.rm = T)) %>% 
  ungroup() %>% 
  # calculate summaries
  pivot_longer(c(si, ti)) %>% 
  # filter out first TI = 0 value
  filter(!(frame_no == 1 & name == "ti" & value == 0)) %>% 
  group_by(dataset, src, name) %>% 
  summarize(
    mean = mean(value),
    min = min(value),
    max = max(value),
    p_upper = quantile(value, 0.975),
    p_lower = quantile(value, 0.025)
  ) %>% 
  pivot_wider(id_cols = c(src, dataset), values_from = where(is_double))

df.siti_summarized %>% 
  filter(dataset == "vqeghd1") %>% 
  ggplot(., aes(x = mean_si, y = mean_ti, label = src, color = src)) +
  geom_point() +
  geom_linerange(aes(ymin = p_lower_ti, ymax = p_upper_ti)) +
  geom_linerange(aes(xmin = p_lower_si, xmax = p_upper_si)) +
  geom_label_repel(family = "Times") +
  xlab("Mean SI") +
  ylab("Mean TI") +
  theme(legend.position = "none") +
  {}
ggsave("plots/mean_si_ti.pdf")

df.siti_summarized %>% 
  filter(dataset == "vqeghd1") %>% 
  ggplot(., aes(x = mean_si, y = mean_ti, label = src)) +
  geom_point() +
  geom_linerange(aes(ymin = p_lower_ti, ymax = p_upper_ti), color = "#CCCCCC") +
  geom_linerange(aes(xmin = p_lower_si, xmax = p_upper_si), color = "#CCCCCC") +
  geom_label_repel(family = "Times") +
  xlab("Mean SI") +
  ylab("Mean TI") +
  theme(legend.position = "none") +
  {}
ggsave("plots/mean_si_ti-bw.pdf", scale = 0.5)

df.siti_summarized %>% 
  filter(dataset == "vqeghd1") %>% 
  mutate(
    si_range = p_upper_si - p_lower_si,
    ti_range = p_upper_ti - p_lower_ti
  ) %>% 
  arrange(desc(si_range))

df.siti_summarized %>% 
  filter(dataset == "vqeghd1") %>% 
  ggplot(aes(x = max_si, y = max_ti, label = src)) +
  geom_point() +
  geom_text_repel(point.padding = 1, family = "Times") +
  xlab("Max SI") +
  ylab("Max TI")
ggsave("plots/max_si_ti.pdf")
