# Libraries

library(tidyverse)

# Load Dataset

player_stats <- read_csv("data/mlb_bat_2024.csv")

# Filter Players who were Traded

traded_players <- player_stats %>% 
  filter(team %in% c("2TM", "3TM", "4TM"))

# Keep Team Specific Rows

non_traded_players <- player_stats %>%
  filter(!team %in% c("2TM", "3TM", "4TM"))


# Save CSV Files

write_csv(traded_players, "traded_players_2024.csv")

write_csv(non_traded_players, "non_traded_players_2024.csv")