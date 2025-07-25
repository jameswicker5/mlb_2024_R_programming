## Library
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(purrr)
library(forcats)

# Load Data
bat_data <- read_csv("C:/Users/james/OneDrive/Desktop/Portfolio/R/data/mlb_bat_2024.csv")

# Filter for Qualified Batters
bat_data <- bat_data %>%
  filter(!is.na(war), !is.na(plate_appearances), plate_appearances > 502)

# Top 10 Players by WAR
top_players <- bat_data %>%
  arrange(desc(war)) %>%
  slice(1:10)

bat_main <- bat_data %>%
  filter(!(player %in% top_players$player))

# Feature Set
features <- c(
  "at_bats", "runs", "hits", "doubles", "triples", "home_runs",
  "runs_batted_in", "stolen_bases", "walks", "strikeouts",
  "batting_average", "on_base_percentage", "slugging_percentage", "total_bases"
)

# Scale Train Data
train_data <- bat_main %>%
  select(all_of(features)) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  scale()

# Split Train/Validation
set.seed(123)
train_index <- sample(1:nrow(train_data), size = 0.8 * nrow(train_data))
train_matrix <- train_data[train_index, ]
valid_matrix <- train_data[-train_index, ]

# Elbow Method Code
wss <- map_dbl(1:10, ~kmeans(train_matrix, centers = ., nstart = 25)$tot.withinss)
elbow_df <- tibble(k = 1:10, wss = wss)
ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color = "steelblue") +
  geom_point(size = 2, color = "darkblue") +
  labs(
    title = "Elbow Method: WSS vs Number of Clusters",
    x = "Number of clusters (k)",
    y = "Total WSS"
  ) +
  theme_minimal()


# Choose clusters (based on elbow)
k_optimal <- 4  # From Elbow Method

# Train K-Means Model
kmeans_model <- kmeans(train_matrix, centers = k_optimal, nstart = 25)

# Predict Function
predict_model <- function(model, new_data) {
  centers <- model$centers
  apply(new_data, 1, function(row) {
    dists <- apply(centers, 1, function(center) sum((row - center)^2))
    which.min(dists)
  })
}

# Top 10 Clustering
top_players_scaled <- top_players %>%
  select(all_of(features)) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  scale(center = attr(train_data, "scaled:center"), scale = attr(train_data, "scaled:scale"))
top10_clusters <- predict_model(kmeans_model, top_players_scaled)
top_players_clustered <- top_players %>%
  mutate(Cluster = top10_clusters) %>%
  select(player, war, Cluster) %>%
  arrange(Cluster)
print(top_players_clustered)

# PCA Projection
valid_labels <- predict_model(kmeans_model, valid_matrix)
pca_model <- prcomp(train_matrix)

pca_train <- as.data.frame(predict(pca_model, train_matrix)) %>%
  mutate(Type = "Train", Cluster = as.factor(kmeans_model$cluster), Player = NA)

pca_valid <- as.data.frame(predict(pca_model, valid_matrix)) %>%
  mutate(Type = "Validation", Cluster = as.factor(valid_labels), Player = NA)

pca_top10 <- as.data.frame(predict(pca_model, top_players_scaled)) %>%
  mutate(Type = "Top10", Player = top_players$player, Cluster = as.factor(top10_clusters))

pca_combined <- bind_rows(pca_train, pca_valid, pca_top10)
explained_var <- round(100 * summary(pca_model)$importance[2, 1:2], 1)

# PCA Plot with Top 10 Labels
ggplot(pca_combined, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(data = filter(pca_combined, Type != "Top10"), alpha = 0.5, size = 2) +
  geom_point(data = filter(pca_combined, Type == "Top10"), shape = 21,
             fill = "gold", color = "black", size = 5, stroke = 1.2) +
  geom_text_repel(
    data = filter(pca_combined, Type == "Top10"),
    aes(label = Player, color = Cluster),
    size = 3.5, fontface = "bold",
    max.overlaps = 100,
    box.padding = 0.75, point.padding = 0.5,
    nudge_y = 0.5, nudge_x = -0.2,
    direction = "both",
    show.legend = FALSE
  ) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "K‑Means Clusters with PCA Projection – MLB Hitting 2024",
    subtitle = "Top 10 Players (WAR) in Gold",
    x = paste0("PC1 (", explained_var[1], "%)"),
    y = paste0("PC2 (", explained_var[2], "%)"),
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
ggsave("pca_kmeans_2024.png", width = 10, height = 6, dpi = 300, bg = "white")

# Cluster Feature Profiling
bat_main_clustered <- bat_main %>%
  mutate(Cluster = predict_model(kmeans_model, scale(select(., all_of(features)))))

cluster_summary <- bat_main_clustered %>%
  group_by(Cluster) %>%
  summarise(across(all_of(features), ~round(mean(.x, na.rm = TRUE), 2)), .groups = "drop")

cluster_summary_scaled <- cluster_summary %>%
  pivot_longer(-Cluster, names_to = "Feature", values_to = "Value") %>%
  group_by(Feature) %>%
  mutate(ZScore = scale(Value)) %>%
  ungroup()

top_features_by_cluster <- cluster_summary_scaled %>%
  group_by(Cluster) %>%
  slice_max(abs(ZScore), n = 3) %>%
  ungroup() %>%
  mutate(Feature = fct_reorder(Feature, abs(ZScore)))

# Bar Plot
ggplot(top_features_by_cluster, aes(x = ZScore, y = Feature, fill = as.factor(Cluster))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Cluster, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Top 3 Hitting Features per Cluster",
    subtitle = "Z-score scaled means",
    x = "Z-Score",
    y = ""
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))
ggsave("cluster_feature_barplot.png", width = 12, height = 6, dpi = 300, bg = "white")
