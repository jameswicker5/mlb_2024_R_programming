# Libraries
library(tidyverse)
library(gt)

# Load Dataset
players <- read_csv("/data/non_traded_players_2024.csv")
team_colors <- read_csv("/data/team_colors.csv")

# Define stat categories
percentage_stats <- c(
  "batting_average", "on_base_percentage", 
  "slugging_percentage", "on_base_plus_slugging"
)

count_stats <- c(
  "war", "games", "plate_appearances", "at_bats", "runs", "hits", "doubles",
  "triples", "home_runs", "runs_batted_in", "stolen_bases", "caught_stealing",
  "walks", "strikeouts", "total_bases", "double_plays", "hit_by_puitch",
  "sacrifice_bunts", "sacrifice_flies", "intentional_walks"
)

# Step 1: Create wide summary
league_summary_wide <- players %>%
  group_by(league) %>%
  summarise(
    across(all_of(count_stats), list(
      tot = ~sum(.x, na.rm = TRUE),
      avg = ~mean(.x, na.rm = TRUE)
    ), .names = "{.fn}_{.col}"),
    
    # Weighted averages for percentages
    avg_batting_average = sum(batting_average * plate_appearances, na.rm = TRUE) / sum(plate_appearances, na.rm = TRUE),
    avg_on_base_percentage = sum(on_base_percentage * plate_appearances, na.rm = TRUE) / sum(plate_appearances, na.rm = TRUE),
    avg_slugging_percentage = sum(slugging_percentage * plate_appearances, na.rm = TRUE) / sum(plate_appearances, na.rm = TRUE),
    avg_on_base_plus_slugging = sum(on_base_plus_slugging * plate_appearances, na.rm = TRUE) / sum(plate_appearances, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Convert to long form
league_summary_long <- league_summary_wide %>%
  pivot_longer(
    cols = -league,
    names_to =  "stat",
    names_sep = "_",
    values_to = "value"
  )

# Step 3: Pivot to wide format with leagues as columns
league_summary_pivot <- league_summary_long %>%
  pivot_wider(
    names_from = league,
    values_from = value
  ) %>%
  arrange(stat)

# Step 4: Preview or export
print(league_summary_pivot)

# Optional: Save to CSV
write_csv(league_summary_pivot, "/data/league_summary_pivot.csv")

# Optional: GT table
league_summary_pivot %>%
  gt() %>%
  tab_header(title = "AL vs NL Summary")
