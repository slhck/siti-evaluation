library(tidyverse)
library(ggplot2)
library(slider)
library(ggrepel)

theme_set(theme_bw() + theme(text=element_text(size=16,  family="Times")))

# Explanatory variables: SI/TI
df.siti = list.files("data/siti", pattern = "csv", full.names = TRUE) %>% 
  map_df(read_csv) %>% 
  rename(src = input_file, frame_no = n) %>% 
  mutate_at("src", tools::file_path_sans_ext) %>% 
  filter(!str_detect(src, "8s")) %>% 
  mutate_at(vars(src), str_replace, "LeagueOfLegends", "League_of_Legends")

df.siti %>% 
  group_by(src) %>% 
  mutate(
    ti_mov_avg = slider::slide_dbl(ti, mean, .before = 120, .after = 0),
    ti_diff = abs(ti - ti_mov_avg)
  ) %>% 
  ggplot(aes(x = src, y = ti_diff)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/scene_cuts.pdf")

df.siti %>% 
  group_by(src) %>% 
  filter(frame_no != 1) %>% 
  group_by(src) %>% 
  # remove scene cuts
  filter(ti < quantile(ti, 0.99)) %>% 
  ungroup() %>% 
  ggplot(aes(x = frame_no, y = ti, color = src)) +
  geom_line()
ggsave("plots/line_plot.pdf")

df.siti_summarized = 
  df.siti %>%
  mutate(src = factor(
    src,
    levels = c(
      "american_football_harmonic",
      "bigbuck_bunny_8bit",
      "cutting_orange_tuil",
      "Sparks_cut_13",
      "Sparks_cut_15",
      "surfing_sony_8bit",
      "vegetables_tuil",
      "water_netflix"
    ),
    labels = c("AFH",
               "BBB",
               "CO",
               "SC13",
               "SC15",
               "SS",
               "VT",
               "WN")
  )) %>%
  # remove scene cuts
  group_by(src) %>% 
  filter(ti < quantile(ti, 0.99)) %>% 
  ungroup() %>% 
  # calculate summaries
  pivot_longer(c(si, ti)) %>% 
  # filter out first TI = 0 value
  filter(!(frame_no == 1 & name == "ti" & value == 0)) %>% 
  group_by(src, name) %>% 
  summarize(
    mean = mean(value),
    min = min(value),
    max = max(value),
    p05 = quantile(value, 0.05),
    p95 = quantile(value, 0.95)
  ) %>% 
  pivot_wider(id_cols = src, values_from = where(is_double))

df.siti_summarized %>% 
  ggplot(aes(x = mean_si, y = mean_ti, label = src)) +
  geom_point() +
  geom_errorbar(aes(ymin = p05_ti, ymax = p95_ti), color = "grey") +
  geom_errorbarh(aes(xmin = p05_si, xmax = p95_si), color = "grey") +
  geom_text_repel(point.padding = 1) +
  xlab("Mean SI") +
  ylab("Mean TI") +
  coord_cartesian(
    xlim = c(0, max(df.siti_summarized$max_si)),
    ylim = c(0, max(df.siti_summarized$max_ti)),
  )
ggsave("plots/mean_si_ti.pdf")

df.siti_summarized %>% 
  ggplot(aes(x = max_si, y = max_ti, label = src)) +
  geom_point() +
  geom_text_repel(point.padding = 1) +
  xlab("Max SI") +
  ylab("Max TI") +
  coord_cartesian(
    xlim = c(0, max(df.siti_summarized$max_si)),
    ylim = c(0, max(df.siti_summarized$max_ti)),
  )
ggsave("plots/max_si_ti.pdf")
