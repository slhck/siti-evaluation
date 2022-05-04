# SITI Analysis

# ========================================================================================================

library(conflicted)
library(pracma)
library(scales)
library(tidyverse)
library(tidylog)
library(lubridate)
library(here)
library(ggrepel)
library(snakecase)
library(ggdark)
library(broom)
library(corrr)
library(colorspace)
library(minpack.lm)

for (f in getNamespaceExports("tidylog")) {
  conflicted::conflict_prefer(f, "tidylog", quiet = TRUE)
}

# turn off tidylog
options("tidylog.display" = list())

conflict_prefer("lag", "dplyr")
conflict_prefer("lead", "dplyr")

source("stat_chull.R")

options(scipen=999)

# ========================================================================================================
# PLOTTING SETUP

colors_custom <- c(
  "#1F7A8C",
  "#D62839",
  "#7EC657",
  "#E5DD49",
  "#E69859",
  "#70798C",
  "#8B63A6",
  "#35478C"
)

scale_fill_custom <- function(...) {
  return(scale_fill_manual(values = colors_custom, ...))
}

scale_color_custom <- function(...) {
  return(scale_color_manual(values = colors_custom, ...))
}

theme_set(theme_minimal())

# save to a sensible size
DEFAULT_PLOT_WIDTH = 4
DEFAULT_PLOT_HEIGHT = 3
DEFAULT_FONT_SIZE = 10

save_plot <-
  function(name,
           p = last_plot(),
           width = DEFAULT_PLOT_WIDTH,
           height = DEFAULT_PLOT_HEIGHT,
           font_size = DEFAULT_FONT_SIZE,
           format = "pdf",
           dpi = 72,
           width_scale = 1,
           height_scale = 1,
           font_scale = 1,
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
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    ...
  )
}

# ========================================================================================================
# DATA READING

read_plus <- function(filename) {
  read_csv(filename) %>% 
    mutate(
      test = basename(dirname(filename))
    )
}

df.metadata = Sys.glob("data/test_*/metadata.csv") %>%
  map_df(~read_plus(.)) %>%
  select(-test) %>%
  unique() %>% 
  # remove duplicate entries due to slightly different bitrates
  group_by(video_name, video_height, video_target_bitrate, video_codec) %>% 
  add_count(video_name) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(-n) %>% 
  mutate(
    video_size = video_bitrate / 8 / 1024 * video_duration,
    video_bitrate_log = log10(video_bitrate)
  ) %>% 
  # FIXME: remove broken Dancers_8s sequence
  filter(src != "Dancers_8s") %>% 
  mutate_at(vars(src), str_replace, "LeagueOfLegends", "League_of_Legends")

df.mos_ci = Sys.glob("data/test_*/mos_ci.csv") %>%
  map_df(~read_plus(.)) %>% 
  # FIXME: remove broken Dancers_8s sequence
  filter(src != "Dancers_8s") %>% 
  mutate_at(vars(src), str_replace, "LeagueOfLegends", "League_of_Legends")

df.tmp.p1204_3_scores = read_csv("data/p1204_3_scores.csv")

df.objective_scores = Sys.glob("data/test_*/objective_scores.csv") %>%
  map_df(~read_plus(.)) %>%
  mutate(across(where(is_double), round, 2)) %>% 
  select(-test) %>%
  unique() %>% 
  # remove duplicate objective scores due to numerical instability
  add_count(video_name) %>% 
  group_by(video_name) %>% 
  arrange(n) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(-n) %>% 
  # add P.1204.3 scores
  left_join(df.tmp.p1204_3_scores) %>% 
  # FIXME: remove broken Dancers_8s sequence
  filter(src != "Dancers_8s") %>% 
  mutate_at(vars(src), str_replace, "LeagueOfLegends", "League_of_Legends") %>% 
  # FIXME: remove broken objective scores for one sequence used, but not p1204.3!
  group_split(src == "water_netflix", .keep = F) %>% 
  modify_at(2, ~{
    mutate(
      .x,
      across(
        where(is_double) & !starts_with("p1204"),
        function(x) NA
      )
    )
  }) %>% 
  bind_rows() %>% 
  group_split(
    video_name %in% c(
      "water_netflix_8s_97kbps_360p_59.94fps_hevc.mp4",
      "water_netflix_200kbps_360p_59.94fps_hevc.mp4",
      "american_football_harmonic_8s_97kbps_360p_59.94fps_h264.mp4",
      "american_football_harmonic_8s_97kbps_360p_59.94fps_hevc.mp4"
    ),
    .keep = F
  ) %>% 
  modify_at(2, ~{
    mutate(
      .x,
      across(
        where(is_double) & starts_with("p1204"),
        function(x) NA
      )
    )
  }) %>% 
  bind_rows()

# FIXME: fix broken vmaf_5_point scores < 1 for the following videos:
# 1 LeagueOfLegends-1_8s_97kbps_360p_60.0fps_h264.mp4                        
# 2 LeagueOfLegends-1_8s_97kbps_360p_60.0fps_vp9.mp4                          
# 3 fr-041_debris_3840x2160_60p_422_ffvhuff_4_8s_200kbps_360p_15.0fps_h264.mp4
# 4 Sparks_cut_15_200kbps_360p_15.0fps_h264.mp4                              
# 5 Sparks_cut_15_500kbps_360p_15.0fps_h264.mp4                              
# 6 Sparks_cut_15_500kbps_360p_24.0fps_h264.mp4                              
# 7 Sparks_cut_15_1000kbps_360p_24.0fps_h264.mp4                              
# 8 Sparks_cut_15_500kbps_480p_15.0fps_h264.mp4                              
# 9 LeagueOfLegends-1_8s_97kbps_360p_60.0fps_hevc.mp4
df.objective_scores = df.objective_scores %>% 
  mutate(vmaf_5_point = scales::rescale(vmaf_score, from = c(0, 100), to = c(1, 5)))

# Explanatory variables: SI/TI
df.siti = list.files("data/siti", pattern = "csv", full.names = TRUE) %>% 
  map_df(read_csv) %>% 
  rename(src = input_file, frame_no = n) %>% 
  mutate_at("src", tools::file_path_sans_ext) %>% 
  mutate_at(vars(src), str_replace, "LeagueOfLegends", "League_of_Legends")

# Calculate some aggregated metrics on SI/TI
df.siti_summary = df.siti %>%
    group_by(src) %>%
    # remove TI = 0 (first frame)
    filter(ti != 0) %>%
    summarize(
      across(
        c(si, ti),
        c(
          mean = mean,
          min = min,
          max = max,
          # median = median,
          # pct05 = ~quantile(.x, 0.05),
          pct25 = ~quantile(.x, 0.25),
          pct50 = ~quantile(.x, 0.50),
          pct75 = ~quantile(.x, 0.75)
          # pct95 = ~quantile(.x, 0.95)
        ),
        .names = "{.col}_{.fn}"
      )
    )

# calculate the criticality metric from Fenimore, Libert, Wolf - 1998 - Perceptual effects of noise in digital video compression
df.siti_criticality = df.siti %>%
  filter(ti != 0) %>% 
  mutate(
    criticality_per_frame = si * ti
  ) %>% 
  group_by(src) %>% 
  summarize(
    criticality = log10(mean(criticality_per_frame))
  )

# ========================================================================================================
# COMBINE TESTS 2 and 3

# get a common set MOS via averaging the sequences that are used more than
# once across the entire set (n > 1)
df.common_set = 
  df.mos_ci %>% 
  select(-src) %>% 
  add_count(video_name) %>% 
  filter(n > 1) %>%
  arrange(video_name) %>% 
  pivot_wider(
    id_cols = video_name,
    names_from = test,
    values_from = MOS,
    names_prefix = "MOS_"
  ) %>% 
  rowwise(video_name) %>% 
  summarize(MOS_common = mean(c_across(starts_with("MOS"))))

# get the model params for the two tests, fitting the original MOS
# against the common MOS based on the common set sequences
df.tmp.mapping_params = df.mos_ci %>% 
  filter(test != "test_1") %>% 
  right_join(df.common_set) %>% 
  group_by(test) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~ lm(MOS_common ~ MOS, data = .x) %>% tidy),
  ) %>% 
  unnest(model) %>% 
  pivot_wider(id_cols = test, names_from = term, values_from = estimate) %>% 
  rename(common_set_intercept = `(Intercept)`, common_set_slope = MOS)

# create a new joint MOS by applying slope and intercept to the original values,
# yielding a MOS_mapped column for the tests 2 and 3
df.mos_ci_mapped =
  df.mos_ci %>% 
  left_join(
    df.mos_ci %>% 
      left_join(df.tmp.mapping_params) %>% 
      mutate(MOS = pmax(pmin(MOS * common_set_slope + common_set_intercept, 5), 1)) %>%
      select(-CI, -src, -starts_with("common_set")) %>% 
      filter(!is.na(MOS)) %>% 
      pivot_wider(names_from = test, values_from = MOS) %>% 
      # re-apply them to the original common set
      left_join(df.common_set) %>% 
      relocate(MOS_common, .before = test_2) %>%
      group_by(video_name) %>% 
      # for the overlapping elements, create a new MOS that is either the
      # averaged (common) MOS, or the individual (mapped) MOS from test 2 and 3
      summarize(MOS_mapped = coalesce(MOS_common, test_2, test_3)) %>% 
      ungroup()  
  )

# generate a set of two tests, test_1 and test_23,
# by dropping the original MOS column if we have a mapped MOS, and changing
# the test name to test_23, then getting the distinct set of results
df.mos_ci_combined = 
  df.mos_ci_mapped %>% 
  relocate(MOS_mapped, .before = MOS) %>% 
  mutate(MOS = coalesce(MOS_mapped, MOS)) %>% 
  mutate(test = if_else(!is.na(MOS_mapped), "test_23", test)) %>% 
  select(-MOS_mapped) %>% 
  unique() %>% 
  filter(test != "test_4")

df.mos_ci_combined %>% left_join(df.metadata) %>% 
  distinct(test, src, video_duration) %>% 
  mutate_at(vars(video_duration), round) %>% 
  count(test, src, video_duration)

# ========================================================================================================
# CONVEX HULLS

df.quality_data =
  df.mos_ci_combined %>% 
  select(-src, -CI) %>% 
  left_join(df.metadata) %>% 
  left_join(
    df.objective_scores %>% select(
      video_name,
      # psnr_score,
      # vmaf_score
      p1204_3_score,
      vmaf_5_point
    )
  ) %>% 
  relocate(MOS, .after = vmaf_5_point) %>% 
  rename(
    VMAF = vmaf_5_point,
    `P.1204.3` = p1204_3_score,
  ) %>% 
  pivot_longer( 
    c(
      VMAF,
      `P.1204.3`,
      MOS
    ), names_to = "score_type", values_to = "score"
  ) %>% 
  unique() %>% 
  filter(!is.na(score))

df.quality_data %>% 
  write_csv("df_quality.csv")

# scatter plots
df.quality_data %>% 
  select(test, video_name, score_type, score) %>% 
  pivot_wider(names_from = score_type, values_from = score) %>% 
  pivot_longer(-c(test, video_name, MOS), names_to = "metric", values_to = "score") %>% 
  ggplot(aes(x = score, y = MOS)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(alpha = 0.3) +
  facet_grid(test~metric)
save_plot("quality_scatter")

# get correlation and RMSE
df.quality_data %>% 
  select(test, video_name, score_type, score) %>% 
  pivot_wider(names_from = score_type, values_from = score) %>% 
  pivot_longer(-c(test, video_name, MOS), names_to = "metric", values_to = "score") %>% 
  filter(!is.na(score)) %>% 
  group_by(metric) %>% 
  summarize(
    correlation = cor(score, MOS),
    rmse = sqrt(mean((MOS - score)^2))
  )

# sanity check: plot the values
df.quality_data %>% 
  group_by(test) %>% 
  group_map(~ {
    p = .x %>% 
      mutate(src = str_replace_all(src, "_", " ")) %>% 
      ggplot(aes(x = video_bitrate, y = score, color = video_codec, linetype = factor(video_height))) +
      geom_point() +
      geom_line() +
      facet_grid(score_type~src, scales = "free_y", labeller = label_wrap_gen(10)) +
      scale_x_log10(breaks = log_breaks(n = 8)) +
      scale_color_custom(name = "Codec") +
      scale_linetype_discrete(name = "Height") +
      # ggtitle(.x$test) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      xlab("Video bitrate")
    show(p)
    save_plot(
      glue::glue("bitrate_ladders-{first(.x$test)}"),
      width_scale = 2.5,
      height_scale = 2.5
    )
  }, .keep = TRUE)

# calculate area under curve for each codec
df.convex_hulls =
  df.quality_data %>% 
  group_by(test, src, video_codec, score_type) %>% 
  nest() %>% 
  mutate(
    # find the convex hull data points
    convex_hull = map(data, function(df) {
      df %>%
        slice(chull(video_bitrate, score)) %>% 
        # remove non-monotonicity manually
        arrange(video_bitrate, score) %>% 
        filter(score > lag(score) | is.na(lag(score))) %>% 
        # FIXME: for some reason we have to do this again?
        arrange(video_bitrate, score) %>% 
        filter(score > lag(score) | is.na(lag(score)))
    })
  ) %>% 
  rename(bitrate_ladder_data = data)

# print convex hulls
df.convex_hulls %>% 
  unnest(convex_hull) %>%
  group_by(test) %>% 
  group_map(~ {
    p = .x %>% 
      mutate(src = str_replace_all(src, "_", " ")) %>% 
      # ggplot(aes(x = video_bitrate, y = score, color = video_codec)) +
      ggplot(aes(x = video_bitrate, y = score, linetype = video_codec, color = video_codec)) +
      geom_line() +
      facet_grid(
        score_type~src,
        scales = "free_y",
        labeller = labeller(src = label_wrap_gen(10))
      ) +
      # ggtitle(.x$test) +
      scale_x_log10(breaks = log_breaks(n = 8)) +
      scale_color_custom(name = "Codec") +
      # scale_color_manual(name = "Codec", values = c("#000000", "#333333", "#999999")) +
      scale_linetype_manual(name = "Codec", values = c("dotted", "dashed", "solid")) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      xlab("Video bitrate")
    show(p)
    save_plot(
      glue::glue("convex_hulls-{first(.x$test)}"),
      width_scale = 1,
      height_scale = 1.1,
      font_scale = 0.9
    )
  }, .keep = TRUE)

# ========================================================================================================
# CONVEX HULL FITTING

third_order_polynomial <- function(video_bitrate, a, b, c, d = 0) {
  return(a * log10(video_bitrate)^3 + b * log10(video_bitrate)^2 + c * log10(video_bitrate) + d)
}

sigmoid_scenic <- function(video_bitrate, a1, a2, a3, a4) {
  return(a1 + (a2 - a1) / (1 + exp(-a3 * (log10(video_bitrate) - a4))))
}

# suppress errors from nlsLM
safe_nlsLM = quietly(nlsLM)

# estimate fit function for hull, based on
# https://www.r-bloggers.com/2020/02/a-collection-of-self-starters-for-nonlinear-regression-in-r/
# https://github.com/OnofriAndreaPG/aomisc
df.convex_hulls_fitted =
  df.convex_hulls %>% 
  unnest(convex_hull) %>% 
  select(-starts_with("quality_area")) %>% # just for cleanup
  group_by(test, src, video_codec, score_type, bitrate_ladder_data) %>% 
  nest() %>% 
  mutate(
    model_poly = map(
      data,
      # second-order polynomial
      # ~ nls(
      #   formula = score ~ a * log10(video_bitrate)^2 + b * log10(video_bitrate) + c,
      #   data = .x,
      #   start = list(a = 1, b = 1, c = 0),
      #   lower = list(a = -1, b = -1, c = 0),
      #   upper = list(a = 5, b = 5, c = 5),
      #   algorithm = "port"
      # )
      # third-order polynomial
      ~ nls(
        formula = score ~ third_order_polynomial(video_bitrate, a, b, c, d),
        data = .x,
        start = list(a = 1, b = 1, c = 1, d = 0),
        lower = list(a = -1, b = -1, c = -5, d = 0),
        upper = list(a = 5, b = 5, c = 5, d = 5),
        algorithm = "port"
      )
    ),
    model_poly_augmented = map(model_poly, augment),
    model_poly_tidy = map(model_poly, tidy),
    model_poly_glance = map(model_poly, glance),
    model_sigmoid = map(
      data,
      ~ safe_nlsLM(
        formula = score ~ sigmoid_scenic(video_bitrate, a1, a2, a3, a4),
        data = .x
      )$result
    ),
    model_sigmoid_augmented = map(model_sigmoid, augment),
    model_sigmoid_tidy = map(model_sigmoid, tidy),
    model_sigmoid_glance = map(model_sigmoid, glance)
  ) %>% 
  rename(convex_hull = data)

# determine the 95% range of video bitrate values for later integration.
# these are per-test and per-codec, over all fitting functions
df.tmp.integration_range =
  df.convex_hulls_fitted %>% 
    unnest(model_sigmoid_augmented) %>%
    ungroup() %>% 
    # we do not consider the range per video codec, but use the entire range
    select(score_type, test, video_bitrate, .fitted) %>% 
    group_by(score_type, test) %>% 
    group_modify(~ {
      .x %>% 
        filter(.fitted >= quantile(.fitted, 0.05)) %>% 
        filter(.fitted <= quantile(.fitted, 0.95)) %>% 
        summarize(
          video_bitrate_range_min = min(video_bitrate),
          video_bitrate_range_max = max(video_bitrate)
        )
    }, .keep = TRUE)

df.convex_hulls_fitted %>% 
  select(group_vars(df.convex_hulls_fitted), model_poly_augmented, model_sigmoid_augmented, -bitrate_ladder_data) %>% 
  pivot_longer(starts_with("model"), names_to = "model") %>% 
  unnest(value) %>% 
  group_by(test, model) %>% 
  group_map(~ {
    p = .x %>% 
      mutate(src = str_replace_all(src, "_", " ")) %>% 
      ggplot(aes(x = video_bitrate, color = video_codec, shape = video_codec)) +
      geom_line(aes(y = .fitted)) +
      geom_point(aes(y = score), alpha = 0.5) +
      facet_grid(
        score_type~src,
        scales = "free_y",
        labeller = labeller(src = label_wrap_gen(10))
      ) +
      # ggtitle(.x$test) +
      scale_x_log10(breaks = log_breaks(n = 8)) +
      scale_color_custom(name = "Codec") +
      scale_shape_discrete(name = "Codec") +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      xlab("Video bitrate") +
      ylab("Score")
    show(p)
    save_plot(
      glue::glue("convex_hulls_fitted-{first(.x$model)}-{first(.x$test)}"),
      width_scale = 1,
      height_scale = 1.2,
      font_scale = 0.9
    )
    save_plot(
      glue::glue("convex_hulls_fitted-{first(.x$model)}-{first(.x$test)}-x2"),
      width_scale = 2,
      height_scale = 2,
      font_scale = 1
    )
  }, .keep = TRUE)

# create a truncated prediction of each model and integrate over that 
df.convex_hulls_with_quality =
  df.convex_hulls_fitted %>% 
  left_join(df.tmp.integration_range) %>% 
  mutate(
    quality_area_poly = map(
      model_poly, function(model) {
        do.call(
          integrate,
          c(list(
              f = third_order_polynomial,
              lower = video_bitrate_range_min,
              upper = video_bitrate_range_max
            ), coef(model))
        )$value
      }
    ) %>% simplify(),
    quality_area_sigmoid = map(
      model_sigmoid, function(model) {
        do.call(
          integrate,
          c(list(
              f = sigmoid_scenic,
              lower = video_bitrate_range_min,
              upper = video_bitrate_range_max
            ), coef(model))
        )$value
      }
    ) %>% simplify()
  ) %>% 
  select(-colnames(df.tmp.integration_range)) %>% 
  # scale quality area from 0-1
  # FIXME: this was initially grouped by test, but should probably
  # be scaled over the entire set of sequences of all tests
  group_by(test, score_type) %>% 
  # group_by(score_type) %>% 
  mutate(across(starts_with("quality_area"), scales::rescale, to = c(0.01, 1))) %>% 
  ungroup() %>% 
  select(-starts_with("model"))

df.mos_ci_combined %>%
  left_join(df.metadata) %>% 
  group_by(test, src, video_codec) %>% 
  summarize(
    MOS = mean(MOS)
  ) %>% 
  ggplot(aes(x = src, y = MOS, fill = video_codec)) +
  geom_col(position = position_dodge()) +
  facet_grid(~test, scales = "free") +
  scale_fill_custom(name = "Codec")
  
# # quality area by video codec
df.convex_hulls_with_quality %>%
  group_by(test) %>%
  group_map(~ {
    p = .x %>%
      mutate(src = str_replace_all(src, "_", " ")) %>% 
      ggplot(aes(x = src, y = quality_area_poly, fill = video_codec)) +
      geom_col(position = position_dodge()) +
      facet_grid(score_type~src, scales = "free", labeller = labeller(src = label_wrap_gen(10))) +
      scale_fill_custom(name = "Codec") +
      theme(
        legend.position = "bottom",
        axis.text.x = element_blank()
      ) + 
      xlab("") +
      ylab("Quality") +
      ggtitle(.x$test)
    show(p)
    save_plot(
      glue::glue("quality_area-per_codec-{first(.x$test)}"),
      width_scale = 1,
      height_scale = 1
    )
  }, .keep = TRUE)
 
# quality area by score type
df.convex_hulls_with_quality %>%
  group_by(test) %>%
  group_map(~ {
    p = .x %>%
      mutate(src = str_replace_all(src, "_", " ")) %>% 
      ggplot(aes(x = src, y = quality_area_poly, fill = score_type)) +
      geom_col(position = position_dodge()) +
      facet_grid(video_codec~src, scales = "free", labeller = labeller(src = label_wrap_gen(10))) +
      scale_fill_custom(name = "Metric") +
      theme(
        legend.position = "bottom",
        axis.text.x = element_blank()
      ) + 
      xlab("") +
      ylab("Quality") +
      ggtitle(.x$test)
    show(p)
    save_plot(
      glue::glue("quality_area-per_score_type-{first(.x$test)}"),
      width_scale = 2.5,
      height_scale = 2.5
    )
  }, .keep = TRUE)

# ========================================================================================================
# SI/TI Evaluation

df.convex_hulls_with_quality %>% 
  pivot_longer(starts_with("quality_area"), names_to = "quality_area_type") %>% 
  # remove list columns from previous runs
  select(-where(is_list)) %>% 
  # harmonize 8s variants
  filter(score_type == "MOS") %>% 
  mutate_at(vars(src), str_replace_all, "_8s", "") %>% 
  group_by(quality_area_type, src, video_codec) %>% 
  summarize(compressibility = mean(value)) %>% 
  ungroup() %>% 
  group_by(quality_area_type) %>% 
  group_map(~ {
    p = .x %>% 
      ggplot(aes(x = video_codec, y = compressibility, fill = video_codec)) +
      geom_col(position = position_dodge()) +
      scale_fill_custom(name = "Video codec") +
      facet_grid(src~., labeller = labeller(src = label_wrap_gen(10), switch = "both")) +
      coord_flip(ylim = c(0, 1)) +
      xlab("Video codec") +
      ylab("Compressibility") +
      theme(
        strip.text.y = element_text(angle = 0),
        legend.position = "none"
      )
    show(p)
    save_plot(glue::glue("compressibility_per_codec-{first(.x$quality_area_type)}"), height_scale = 0.9)
  }, .keep = TRUE)

# generate the compressibility score from the quality area
df.siti_impact =
  df.convex_hulls_with_quality %>% 
  mutate(compressibility = quality_area_sigmoid) %>% 
  select(-starts_with("quality_area")) %>% 
  # remove list columns from previous runs
  select(-where(is_list)) %>% 
  left_join(df.siti_summary) %>% 
  left_join(df.siti_criticality)

# correlation over all sources, per score type and codec
df.siti_impact.correlated =
  df.siti_impact %>% 
  group_by(score_type, video_codec) %>% 
  nest() %>% 
  mutate(
    # correlate all values to compressibility
    correlated = map(data, function(df) {
      df %>% 
        select(where(is_double)) %>% 
        correlate(quiet = T)
    }),
    correlated_compressibility = map(correlated, function(df) {
      df %>%
        focus(compressibility) %>% 
        arrange(desc(compressibility)) %>% 
        rename(correlation = compressibility)
    })
  )

df.siti_impact.correlated %>% 
  unnest(correlated_compressibility) %>% 
  mutate_at("term", str_replace_all, "_", " ") %>% 
  mutate_at("term", str_to_upper) %>% 
  mutate(is_si = stringr::str_starts(term, "SI")) %>% 
  mutate(is_pct = stringr::str_detect(term, "PCT")) %>% 
  mutate(is_criticality = stringr::str_detect(term, "CRIT")) %>% 
  # group_by(term) %>% 
  # mutate(average_correlation = mean(correlation)) %>% 
  # ungroup() %>% 
  # ggplot(aes(x = reorder(term, average_correlation), y = correlation)) +
  ggplot(aes(x = term, y = correlation, fill = interaction(is_si, is_pct, is_criticality))) +
  geom_col(position = position_dodge()) +
  facet_grid(video_codec~score_type) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_reverse() +
  scale_fill_manual(values = c(
    colors_custom[1],
    colors_custom[2],
    colorspace::darken(colors_custom[1], 0.5),
    colorspace::darken(colors_custom[2], 0.5),
    colors_custom[3]
  ), name = "SI") +
  xlab("") +
  ylab("") +
  coord_flip()
save_plot("compressibility_correlation", width_scale = 1, height_scale = 1.8)
