library(tidyverse)

# -----------------------------
# 1) Load data
# -----------------------------
schedule <- read_csv("/data/mlb_schedule_2024.csv", show_col_types = FALSE)

# -----------------------------
# 2) Matchup counter
# -----------------------------
matchup_counts <- schedule %>%
  count(home, away, name = "games") %>%
  arrange(desc(games))

# -----------------------------
# 3) Total games per team
# -----------------------------
team_totals <- schedule %>%
  pivot_longer(cols = c(home, away), names_to = "venue", values_to = "team") %>%
  count(team, name = "total_games") %>%
  arrange(desc(total_games))

# -----------------------------
# 4) Save outputs
# -----------------------------
write_csv(matchup_counts, "mlb_matchups_2024.csv")
write_csv(team_totals, "mlb_team_game_totals.csv")
