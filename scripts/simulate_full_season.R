# ------------------------------------------------------------------
# Simulate full MLB season from a fixed schedule file (updated for pitcher-batter engine)
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(gt)
})

# ---------------------------
# 0) Paths / setup
# ---------------------------
scripts_dir   <- "C:/Users/james/OneDrive/Desktop/Portfolio/R/scripts"
schedule_path <- "C:/Users/james/OneDrive/Desktop/Portfolio/R/data/mlb_schedule_2024.csv"

setwd(scripts_dir)

if (!file.exists("setup_simulation.R")) {
  stop("setup_simulation.R not found in working directory: ", getwd())
}
source("setup_simulation.R")  # loads all sim functions + data

set.seed(42)

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

# Build 9-man lineups
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

stopifnot(all(c("home", "away") %in% names(schedule_df)))

if (!"date" %in% names(schedule_df)) {
  schedule_df <- schedule_df %>% mutate(date = row_number())
}

# Expand teams list if needed
schedule_teams <- union(schedule_df$home, schedule_df$away)
not_in_divisions <- setdiff(schedule_teams, teams)
if (length(not_in_divisions)) {
  message("⚠ Teams in schedule but not in division list: ", paste(not_in_divisions, collapse = ", "))
  teams <- union(teams, schedule_teams)
}

# ---------------------------
# 3) Fill missing lineups
# ---------------------------
missing_lineups <- setdiff(teams, names(team_lineups))
if (length(missing_lineups)) {
  message("⚠ Missing lineups for: ", paste(missing_lineups, collapse = ", "),
          ". Copying a random existing lineup for each.")
  for (t in missing_lineups) {
    team_lineups[[t]] <- team_lineups[[sample(names(team_lineups), 1)]]
  }
}

# ---------------------------
# 4) Simulate games (using pitcher+batter engine)
# ---------------------------
message("Simulating ", nrow(schedule_df), " games with pitching matchups...")

# Track starting pitcher rotation index
starter_counters <- setNames(rep(1, length(teams)), teams)

simulate_game_row <- function(home, away, date) {
  idx_home <- starter_counters[[home]]
  idx_away <- starter_counters[[away]]

  game_result <- simulate_game_pitching(
    lineup1 = team_lineups[[home]],
    lineup2 = team_lineups[[away]],
    team1 = home,
    team2 = away,
    starter_index_1 = idx_home,
    starter_index_2 = idx_away
  )

  # Update rotation counters (cycle 1-5)
  starter_counters[[home]] <<- (idx_home %% rotation_n) + 1
  starter_counters[[away]] <<- (idx_away %% rotation_n) + 1

  tibble(
    date = date,
    team = home,
    opponent = away,
    runs_for = game_result$Team1,
    runs_against = game_result$Team2,
    win = game_result$Team1 > game_result$Team2
  )
}

season_home <- pmap_dfr(schedule_df, simulate_game_row)

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

# Division standings
division_lookup <- map_dfr(divisions, ~ tibble(team = .x), .id = "division")

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
  tab_header(title = "Simulated 2024 MLB Season (with Pitching)") %>%
  fmt_number(c(Games, Wins, Losses, Runs_For, Runs_Against, Run_Diff), decimals = 0) %>%
  fmt_number(Win_Pct, decimals = 3)
