library(tidyverse)
library(ggpointdensity)
library(tidylog)
library(ggrepel)
library(cowplot)
library(ggforce)
library(corrr)
library(viridis)
library(RColorBrewer)

# ==============================================================================
# HELPER FUNCTIONS

read_plus <- function(flnm) {
  read_csv(flnm, show_col_types = FALSE) %>% 
    dplyr::mutate(filename = flnm)
}

# create normalized versions by scaling to 0-1 across entire dataset
normalize_cols <- function(df) {
  return (df %>% 
            mutate(across(
              .cols = where(is.double) & !one_of("n"),
              .fns = ~ scales::rescale(.x, to = c(0, 1)),
              .names = "{.col}_normalized"
            )))
}

# ==============================================================================
# PLOT FUNCTIONS

save_plot <-
  function(name,
           p = last_plot(),
           format = "pdf",
           font_size = 10,
           ...) {
    if (width_scale != 1) {
      width = DEFAULT_PLOT_WIDTH * width_scale
    }
    if (height_scale != 1) {
      height = DEFAULT_PLOT_HEIGHT * height_scale
    }
    if (font_scale != 1) {
      font_size = DEFAULT_FONT_SIZE * font_scale
    }
    
    ggsave(
      plot = p + theme(text = element_text(size = font_size)),
      device = ifelse(format == "pdf", "pdf", NULL),
      filename = here::here("figures", paste0(name, ".", format)),
      ...
    )
  }

# ==============================================================================
# READ DATA

# SI/TI
df.siti = list.files("data/siti/", pattern = "*.csv", full.names = T) %>% 
  map_df(~read_plus(.)) %>% 
  select(-filename) %>% # it's already in the dataset itself
  mutate(input_file = tools::file_path_sans_ext(input_file))

# VCA
df.vca = list.files("data/vca/", pattern = "*-complexity.csv", full.names = T) %>% 
  map_df(~read_plus(.)) %>%
  mutate(input_file = tools::file_path_sans_ext(basename(filename))) %>% 
  mutate(input_file = gsub("-complexity", "", input_file)) %>% 
  select(-filename) %>% 
  rename(n = POC) %>% 
  mutate(n = n+1) %>% 
  relocate(input_file) %>% 
  rename_with(~ paste0("vca_", .x), .cols = where(is.double) & !one_of("n"))

# combine data
df.all = df.siti %>% left_join(df.vca)

# convert into long format
df.all_long = df.all %>% 
  select(input_file, n, si, ti, vca_E, vca_h) %>% 
  normalize_cols() %>% 
  pivot_longer(-one_of(c("input_file", "n")), names_to = "indicator")

# ==============================================================================
# PLOTS

df.all_long %>% 
  filter(str_detect(indicator, "normalized")) %>% 
  filter(input_file == "NETFLIX_ElFuente_1920x1080_14296frames_2997fps_000832_000953") %>% 
  ggplot(aes(x = n, y = value, color = indicator)) +
  geom_line()

df.all %>% 
  select(-input_file, -n) %>% 
  correlate() %>% 
  rplot(
    colors = c(brewer.pal(3, name = "RdYlGn"))
  )
