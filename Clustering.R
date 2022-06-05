library(tidyverse)
library(cluster)
library(umap)
library(gt)

set.seed(1)

# Columns needed
needed_columns <- c("pitch_name", "release_speed",
                    "pfx_x_pv_adj", "pfx_z",
                    "release_spin_rate")

# Load game data
data <- tRead::load_seasons(2021) |> 
  filter(game_type == "R") |> 
  group_by(pitcher) |> 
  filter(n() >= 150, !pitch_name %in% c("Eephus", "Fastball", "Screwball")) |> 
  ungroup() |> 
  drop_na(all_of(needed_columns)) |> 
  tRead::add_est_spin_efficiency() |> 
  drop_na(est_spin_efficiency)
  
  # Find "average" FB
  p_avgs <- data |>
  group_by(game_year, pitcher) |>
  top_frac(0.10, release_speed) |> # Top 10% of hardest pitches thrown are used as the av. FB
  summarize(avg_velo = mean(release_speed, na.rm=TRUE))

# Combine data with "averages"
raw_data <- data |> 
  left_join(p_avgs,  by = c("game_year", "pitcher")) |> 
  mutate(velo_ratio = if_else(release_speed/avg_velo > 1, 1, release_speed/avg_velo))

# Getting pitch averages differences
cleaned_mlb <- raw_data |>
  group_by(pitcher, player_name, pitch_name, pitch_type) |> 
  summarize(avg_velo_ratio = mean(velo_ratio, na.rm = TRUE)*100,
            avg_horz = mean(pfx_x_pv_adj, na.rm = TRUE),
            avg_vert = mean(pfx_z, na.rm = TRUE),
            avg_eff = mean(est_spin_efficiency, na.rm = TRUE)) |> 
  ungroup()


# Clustering----------------------------------------------------------------------------------------
# 
# cluster_data <- cleaned_mlb |> 
#   select("avg_velo_ratio", "avg_vert", "avg_horz", "avg_eff")
# 
# # Already had stored values from previous code
# cleaned_clusters <- pam(cluster_data,
#                         k = 17, metric = "euclidean")
# 
# write_csv(cleaned_clusters$medoids |> as_tibble(), "./Medoids.csv")
# 






# Function testing----------------------------------------------------------------------------------

# Loading saved medoids
saved_clusters <- read_csv("./Medoids.csv")

# Functions to return pitch cluster
eucDist <- function(x, y) sqrt(sum( (x-y)^2 ))

classifyNewSample <- function(newData, centroids = saved_clusters) {
  dists = apply(centroids, 1, function(y) eucDist(y,newData))
  order(dists)[1]
}

# Backup cluster data definition in case the full model isn't being run
cluster_data <- cleaned_mlb |>
  select("avg_velo_ratio", "avg_vert", "avg_horz", "avg_eff")


# Add clusters based on saved Medoids
mlb_clusters <- cleaned_mlb |> 
  mutate(cluster = apply(cluster_data, 1, classifyNewSample),
         cluster = as.factor(cluster))

# Analyze pitch_names with clusters
table(mlb_clusters$cluster, mlb_clusters$pitch_type)


# Find movement averages for each cluster
cluster_mov_avgs <- mlb_clusters |>
  mutate(number_cluster = as.factor(cluster)) |>
  group_by(number_cluster) |>
  summarize(av_x = mean(avg_horz),
            av_y = mean(avg_vert))

# Plot Clusters found
mlb_clusters |>
  ggplot(aes(x = avg_horz, y = avg_vert)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color = cluster)) +
  geom_label(data = cluster_mov_avgs, aes(x = av_x, y = av_y, label = number_cluster)) +
  scale_color_discrete() +
  coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25)) +
  tRead::theme_tread_538() +
  labs(title = "Pitch Clusters",
       subtitle = "K-means Clustering for 2021 MLB Pitch Types | Normalized to RHP's POV | Labels represent cluster center",
       x = "Horizontal Break (in.)",
       y = "Induced Vertical Break (in.)",
       color = "Cluster"
       ) +
  theme(legend.position = "right", legend.direction = "vertical")

#ggsave("./Images/Movement Clusters.png", width = 10, height = 8, dpi = "retina")


# Plot MLBAM pitch_names along with cluster labels
mlb_clusters |>
  ggplot(aes(x = avg_horz, y = avg_vert)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color = pitch_name)) +
  geom_label(data = cluster_mov_avgs, aes(x = av_x, y = av_y, label = number_cluster)) +
  scale_color_discrete() +
  coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25)) +
  tRead::theme_tread_538() +
  labs(
    title = "Pitch Clusters",
    subtitle = "K-means Clustering for 2021 MLB Pitch Types | Normalized to RHP's POV | Labels represent cluster center",
    x = "Horizontal Break (in.)",
    y = "Induced Vertical Break (in.)",
    color = "Pitch Name"
  ) +
  theme(legend.position = "right", legend.direction = "vertical")

#ggsave("./Images/Movement Clusters by Pitch Name.png", width = 10, height = 8, dpi = "retina")

# UMAP----------------------------------------------------------------------------------------------
# Already have UMAP Image from previous script analysis
# 
# custom_config <- umap.defaults
# 
# custom_config$n_neighbors = 200
# custom_config$min_dist = .4
# 
# umap_data <- mlb_clusters |> 
#   select("avg_velo_ratio", "avg_vert", "avg_horz", "avg_eff")
# 
# umap_testing <- umap(umap_data, config = custom_config)
# 
# umap_plot_data <- mlb_clusters |>
#   mutate(x = umap_testing$layout[,1],
#          y = umap_testing$layout[,2]
#          )
# 
# cluster_avgs <- umap_plot_data |>
#   group_by(cluster) |>
#   summarize(av_x = mean(x),
#             av_y = mean(y))
# 
# umap_plot_data |>
#   ggplot(aes(x = x, y = y)) +
#   geom_point(aes(color = pitch_name), size = 2) +
#   geom_label(data = cluster_avgs, aes(x = av_x, y = av_y, label = cluster)) +
#   tRead::theme_tread_538() +
#   labs(
#     color = "MLBAM Pitch Name",
#     title = "UMAP Dimension Reduction"
#   ) +
#   scale_color_brewer(palette = "Set1") +
#   theme(panel.grid.major = element_blank(), 
#         axis.text = element_blank(), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_blank())
# 
# ggsave("./Images/UMAP Clusters.png", width = 10, height = 8, dpi = "retina")


# MLB Data Testing----------------------------------------------------------------------------------

# Create a map of pitchers and the clusters each of their pitches belong to
pitcher_pitch_map <- mlb_clusters |> 
  select(pitcher, player_name, pitch_name, cluster)

# Combine the map with raw data and find averages for each pitch type
combined_data <- raw_data |> 
  left_join(pitcher_pitch_map) |> 
  mutate(adj_spin_axis = if_else(p_throws == "R", spin_axis, 360-spin_axis)) |> 
  group_by(pitcher, pitch_name, cluster) |> 
  summarize(velo_ratio = mean(velo_ratio, na.rm = TRUE),
            pfx_z = mean(pfx_z, na.rm = TRUE),
            pfx_x_pv_adj = mean(pfx_x_pv_adj, na.rm = TRUE),
            est_spin_efficiency = mean(est_spin_efficiency, na.rm = TRUE),
            release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
            adj_spin_axis = mean(adj_spin_axis, na.rm = TRUE)) |> 
  ungroup()

# Turn the combined data into a longer df for plotting with a facet_wrap
filtered_data <- combined_data |> 
  select(cluster, velo_ratio, pfx_z, pfx_x_pv_adj, est_spin_efficiency, release_spin_rate, adj_spin_axis)

long_data <- filtered_data |> 
  pivot_longer(!cluster, names_to = "metric", values_to = "value")

# Plot and save
long_data |> 
  ggplot(aes(x = cluster, y = value)) +
  geom_violin(aes(fill = cluster), color = "black", show.legend = FALSE) +
  geom_boxplot(width = 0.25, outlier.shape = NA) +
  facet_wrap(~metric, scales = "free_y") +
  tRead::theme_tread_538() +
  theme(strip.text = element_text(face = "bold")) +
  labs(x = "Cluster", 
       y = "Value",
       title = "Metric Ranges By Cluster",
       subtitle = "Movement and Spin Axis Adjusted to RHP POV")

#ggsave("./Images/Cluster Violin Plot.png", width = 14, height = 8, dpi = "retina")

# Find averages for each cluster to turn into a chart
tabular_data <- combined_data |> 
  group_by(cluster) |> 
  summarize(velo_ratio = mean(velo_ratio, na.rm = TRUE),
            pfx_z = mean(pfx_z, na.rm = TRUE),
            pfx_x_pv_adj = mean(pfx_x_pv_adj, na.rm = TRUE),
            est_spin_efficiency = mean(est_spin_efficiency, na.rm = TRUE),
            release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
            adj_spin_axis = mean(adj_spin_axis, na.rm = TRUE)) |> 
  ungroup()

# Create gt chart
tabular_data |> 
  arrange(-velo_ratio) |> 
  gt() |> 
  gtExtras::gt_theme_538() |> 
  cols_label(cluster = "Cluster",
             velo_ratio = "Velo Ratio",
             pfx_z = "Ind. Vert. Break (in.)",
             pfx_x_pv_adj = "Adj. Horz. Break (in.)",
             est_spin_efficiency = "Est. Spin Efficiency",
             release_spin_rate = "Spin Rate",
             adj_spin_axis = "Adj. Spin Axis") |> 
  fmt_number(columns = c(release_spin_rate, adj_spin_axis),
             decimals = 0, use_seps = FALSE) |>
  fmt_number(columns = c(pfx_z, pfx_x_pv_adj),
             decimals = 2) |> 
  fmt_percent(columns = c(velo_ratio, est_spin_efficiency),
              decimals = 1) |> 
  tab_style(style = list(cell_borders(side = "right", color = "black", weight = px(2))),
            locations = cells_body(columns = c(est_spin_efficiency))) |> 
  tab_header(title = "Pitch Metric Averages By Cluster",
             subtitle = "Movement And Spin Axis Adjusted To RHP POV | Arranged By Descending Velo Ratio") |> 
  opt_align_table_header("center") |> 
  tab_footnote("Only first four metrics used in clustering model") #|> 
  #gtsave("./Images/Cluster Metric Averages.png")



# Load in pitch result stats from baseball savant
# https://baseballsavant.mlb.com/leaderboard/pitch-arsenal-stats?type=pitcher&pitchType=&year=2021&team=&min=10
pitch_arsenal_stats <- read_csv("./pitch-arsenal-stats.csv") |> 
  mutate(pitch_name = case_when(pitch_name == "4-Seamer" ~ "4-Seam Fastball",
                                pitch_name == "Splitter" ~ "Split-Finger",
                                TRUE ~ pitch_name))

# Combine dataframes with standardized pitch names
results_data <- pitcher_pitch_map |> 
  mutate(pitch_name = if_else(pitch_name == "Knuckle Curve", "Curveball", pitch_name)) |> 
  left_join(pitch_arsenal_stats, by = c("pitcher" = "player_id", "pitch_name"))

# Create gt chart
results_data |> 
  drop_na() |> 
  group_by(cluster) |> 
  summarize(n = sum(pitches),
            rv_per_100 = mean(run_value_per_100, na.rm = TRUE),
            woba = mean(woba, na.rm = TRUE),
            xwoba = mean(est_woba, na.rm = TRUE),
            whiff_perc = mean(whiff_percent, na.rm = TRUE),
            put_away_rate = mean(put_away, na.rm = TRUE),
            hh_per = mean(hard_hit_percent, na.rm = TRUE)) |> 
  gt() |> 
  gtExtras::gt_theme_538() |> 
  cols_label(n = "# Thrown", 
             cluster = "Cluster",
             rv_per_100 = "RV/100",
             woba = "wOBA",
             xwoba = "xwOBA",
             whiff_perc = "Whiff %",
             put_away_rate = "Put Away Rate",
             hh_per = "Hard Hit %") |> 
  fmt_percent(columns = c(whiff_perc, put_away_rate, hh_per), decimals = 1, scale_values = FALSE) |> 
  fmt_number(columns = c(rv_per_100, woba, xwoba), decimals = 3) |> 
  gtExtras::gt_hulk_col_numeric(columns = rv_per_100, reverse = TRUE) |> 
  tab_header(title = "Pitch Results By Cluster",
             subtitle = "Results averaged across pitchers and are not weighted by # of pitches thrown") |> 
  opt_align_table_header("center") #|> 
  #gtsave("./Images/Cluster Results Averages.png")



# Manually naming new pitch group based on the clusters
# Labeling them as pitch groups because they all need more descriptive names 
# e.g. gyro and sweepy sliders
manually_set_cluster_names <- tribble(~cluster, ~pitch_group,
                                      1, "Fastball",
                                      2, "Changeup-Splitter",
                                      3, "Curveball",
                                      4, "Slutter",
                                      5, "Fastball",
                                      6, "Sinker",
                                      7, "Slider",
                                      8, "Changeup-Splitter",
                                      9, "Slider",
                                      10, "Fastball",
                                      11, "Cutter",
                                      12, "Slider",
                                      13, "Sinker",
                                      14, "Fastball",
                                      15, "Curveball",
                                      16, "Changeup-Splitter",
                                      17, "Curveball") |> 
  mutate(pitch_class = case_when(pitch_group %in% c("Fastball", "Sinker", "Cutter") ~ "Fastball",
                                 pitch_group == "Changeup-Splitter" ~ "Offspeed",
                                 TRUE ~ "Breaking Ball"),
         cluster = as.factor(cluster))


# Find movement averages for each cluster
cluster_mov_avgs <- mlb_clusters |>
  mutate(number_cluster = as.factor(cluster)) |>
  group_by(number_cluster) |>
  summarize(av_x = mean(avg_horz),
            av_y = mean(avg_vert))

# Plot new pitch groups along with cluster labels
mlb_clusters |>
  left_join(manually_set_cluster_names) |> 
  ggplot(aes(x = avg_horz, y = avg_vert)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color = pitch_group)) +
  geom_label(data = cluster_mov_avgs, aes(x = av_x, y = av_y, label = number_cluster)) +
  scale_color_discrete() +
  coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25)) +
  tRead::theme_tread_538() +
  labs(
    title = '"New" Pitch Names',
    subtitle = "Normalized to RHP's POV | Labels represent cluster center",
    x = "Horizontal Break (in.)",
    y = "Induced Vertical Break (in.)",
    color = "Pitch Name"
  ) +
  theme(legend.position = "right", legend.direction = "vertical")

#ggsave("./Images/Movement Clusters by New Pitch Group.png", width = 10, height = 8, dpi = "retina")



