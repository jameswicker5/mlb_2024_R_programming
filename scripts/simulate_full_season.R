# ------------------------------------------------------------------
# Simulate full MLB season from a fixed schedule file
# Each row in mlb_season_2024.csv is a game: columns = home, away
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(gt)
})

# ---------------------------
# 0) Paths / setup
# ---------------------------
# EDIT these if needed
scripts_dir   <- "C:/Users/james/OneDrive/Desktop/Portfolio/R/scripts"
schedule_path <- "C:/Users/james/OneDrive/Desktop/Portfolio/R/data/mlb_schedule_2024.csv"  # <— your new file

setwd(scripts_dir)

if (!file.exists("setup_simulation.R")) {
  stop("setup_simulation.R not found in working directory: ", getwd())
}
source("setup_simulation.R")  # must define sim_players, simulate_game(), etc.

set.seed(42)  # reproducibility

# ---------------------------
# 1) Team list / lineups
# ---------------------------
divisions <- list(
  AL_East     = c("BAL","BOS","NYY","TBR","TOR"),
  AL_Central  = c("CHW","CLE","DET","KCR","MIN"),
  AL_West     = c("HOU","LAA","SEA","TEX","OAK"),
  NL_East     = c("ATL","MIA","NYM","PHI","WSN"),
  NL_Central  = c("CHC","CIN","MIL","PIT","STL"),
  NL_West     = c("ARI","COL","LAD","SDP","SFG")
)
teams <- unlist(divisions)

# Build 9-man lineups (fallback handled below if any are missing)
team_lineups <- sim_players %>%
  filter(team %in% teams) %>%
  group_by(team) %>%
  slice_max(plate_appearances, n = 9, with_ties = FALSE) %>%
  ungroup() %>%
  split(.$team)

# ---------------------------
# 2) Load schedule
# ---------------------------
schedule_df <- read_csv(schedule_path, show_col_types = FALSE)

# Expecting columns 'home' and 'away'
stopifnot(all(c("home", "away") %in% names(schedule_df)))

# If there is a 'date' column, keep it; else create a sequential game_id
if (!"date" %in% names(schedule_df)) {
  schedule_df <- schedule_df %>%
    mutate(date = row_number())
}

# Check every team appears in your team set
schedule_teams <- union(schedule_df$home, schedule_df$away)

not_in_divisions <- setdiff(schedule_teams, teams)
if (length(not_in_divisions)) {
  message("⚠ The following teams are in the schedule but not in your divisions vector: ",
          paste(not_in_divisions, collapse = ", "))
  # You can choose to stop or just expand 'teams' to include these:
  teams <- union(teams, schedule_teams)
}

# ---------------------------
# 3) Fill missing lineups (if any)
# ---------------------------
missing_lineups <- setdiff(teams, names(team_lineups))
if (length(missing_lineups) > 0) {
  message("⚠ Missing lineups for: ", paste(missing_lineups, collapse = ", "),
          ". Will copy a random existing lineup for each.")
  for (t in missing_lineups) {
    team_lineups[[t]] <- team_lineups[[sample(names(team_lineups), 1)]]
  }
}

# ---------------------------
# 4) Simulate games
# ---------------------------
message("Simulating ", nrow(schedule_df), " games...")

season_home <- schedule_df %>%
  rowwise() %>%
  mutate(res = list(simulate_game(team_lineups[[home]], team_lineups[[away]]))) %>%
  unnest_wider(res) %>%
  ungroup() %>%
  transmute(
    date,
    team = home,
    opponent = away,
    runs_for = Team1,
    runs_against = Team2,
    win = Team1 > Team2
  )

season_away <- season_home %>%
  transmute(
    date,
    team = opponent,
    opponent = team,
    runs_for = runs_against,
    runs_against = runs_for,
    win = !win
  )

season <- bind_rows(season_home, season_away)

# ---------------------------
# 5) Standings
# ---------------------------
standings <- season %>%
  group_by(team) %>%
  summarise(
    Games = n(),
    Wins = sum(win),
    Losses = Games - Wins,
    Win_Pct = round(Wins / Games, 3),
    Runs_For = sum(runs_for),
    Runs_Against = sum(runs_against),
    Run_Diff = Runs_For - Runs_Against,
    .groups = "drop"
  ) %>%
  arrange(desc(Wins), desc(Run_Diff))

# Division standings -- Used for Factoring Postseason

# Create a division lookup table from the list
division_lookup <- map_dfr(divisions, ~ tibble(team = .x), .id = "division")

# Join and sort
standings_div <- standings %>%
  left_join(division_lookup, by = "team") %>%
  arrange(division, desc(Wins), desc(Run_Diff))

# ---------------------------
# 6) Save & Display
# ---------------------------
write_csv(season,    "season_game_results.csv")
write_csv(standings, "season_standings.csv")
standings_div %>%
  group_split(division) %>%
  walk(~ write_csv(.x, paste0("standings_", unique(.x$division), ".csv")))

standings %>%
  gt() %>%
  tab_header(title = "Simulated 2024 MLB Season") %>%
  fmt_number(c(Games, Wins, Losses, Runs_For, Runs_Against, Run_Diff), decimals = 0) %>%
  fmt_number(Win_Pct, decimals = 3)
