# setup_simulation.R  (pitcher rates, no modifiers)

suppressPackageStartupMessages({
  library(tidyverse)
  library(gt)
  library(purrr)
})

# ------------------------------------------------------------------
# 0) Load batting & pitching data
# ------------------------------------------------------------------
bat_path   <- "C:/Users/james/OneDrive/Desktop/Portfolio/R/data/mlb_bat_2024.csv"
pitch_path <- "C:/Users/james/OneDrive/Desktop/Portfolio/R/data/mlb_pitch_2024.csv"

bat_data <- read_csv(bat_path, show_col_types = FALSE) %>%
  filter(!team %in% c("2TM", "3TM"))

pitch_data_raw <- read_csv(pitch_path, show_col_types = FALSE) %>%
  filter(!team %in% c("2TM", "3TM", "4TM"))

# ---- Rename every pitching column to short, code-friendly names ----
pitch_data <- pitch_data_raw %>%
  rename(
    player     = player,
    year       = year,
    age        = age,
    team       = team,
    league     = league,
    war        = war,
    wins       = wins,
    losses     = losses,
    win_pct    = win_loss_percentage,
    era        = earned_run_average,
    g          = games,
    gs         = games_started,
    gf         = games_finished,
    cg         = complete_games,
    sho        = shoutouts,
    sv         = saves,
    ip         = innings_pitched,
    h          = hits,
    r          = runs,
    er         = earned_runs,
    hr         = home_runs,
    bb         = walks,
    ibb        = intentional_walks,
    so         = strikeouts,
    hbp        = hit_by_pitch,
    bk         = balks,
    wp         = wild_pitches,
    bf         = batters_faced,
    era_plus   = era_plus,
    fip        = fip,
    whip       = whip,
    h9         = hits_per9,
    hr9        = home_runs_per9,
    bb9        = walks_per9,
    so9        = strikeouts_per9,
    so_bb      = strikeout_walk_ratio
  )

# ------------------------------------------------------------------
# 1) Batter event probabilities
# ------------------------------------------------------------------
sim_players <- bat_data %>%
  filter(plate_appearances >= 50) %>%
  select(player, team, plate_appearances, at_bats, hits, doubles, triples, home_runs, walks, strikeouts) %>%
  mutate(
    singles = hits - doubles - triples - home_runs,
    outs    = at_bats - hits,
    events  = singles + doubles + triples + home_runs + walks + outs
  ) %>%
  rowwise() %>%
  mutate(
    probs = list(c(
      singles   / events,
      doubles   / events,
      triples   / events,
      home_runs / events,
      walks     / events,
      outs      / events
    )),
    outcomes = list(c("singles", "doubles", "triples", "home_runs", "walks", "outs"))
  ) %>%
  ungroup()

# ------------------------------------------------------------------
# 2) Pitcher event probabilities (direct rates)
# ------------------------------------------------------------------
# Pitchers don't have 2B/3B in your CSV, so split non-HR hits by league batting distribution
lg_hit_shares <- bat_data %>%
  mutate(
    singles = hits - doubles - triples - home_runs
  ) %>%
  summarise(
    lg_1b = sum(singles),
    lg_2b = sum(doubles),
    lg_3b = sum(triples),
    lg_HR = sum(home_runs)
  ) %>%
  mutate(
    non_hr_total = lg_1b + lg_2b + lg_3b,
    w1b = lg_1b / non_hr_total,
    w2b = lg_2b / non_hr_total,
    w3b = lg_3b / non_hr_total
  )

w1b <- lg_hit_shares$w1b
w2b <- lg_hit_shares$w2b
w3b <- lg_hit_shares$w3b

pitcher_probs <- pitch_data %>%
  filter(!is.na(bf), bf > 0) %>%
  mutate(
    non_hr_hits = pmax(h - hr, 0),
    singles = non_hr_hits * w1b,
    doubles = non_hr_hits * w2b,
    triples = non_hr_hits * w3b,
    # outs = BF - (BB + H)
    outs = pmax(bf - (bb + h), 0),
    total = singles + doubles + triples + hr + bb + outs
  ) %>%
  filter(total > 0) %>%
  rowwise() %>%
  mutate(
    probs = list(c(
      singles / total,
      doubles / total,
      triples / total,
      hr      / total,
      bb      / total,
      outs    / total
    )),
    outcomes = list(c("singles", "doubles", "triples", "home_runs", "walks", "outs"))
  ) %>%
  ungroup() %>%
  select(player, team, gs, bf, probs, outcomes)

# ------------------------------------------------------------------
# 3) Rotation & bullpen (use pitcher *rates*)
# ------------------------------------------------------------------
rotation_n <- 5

rotation_tbl <- pitch_data %>%
  select(player, team, gs, bf) %>%
  arrange(team, desc(gs), desc(bf)) %>%
  group_by(team) %>%
  slice_head(n = rotation_n) %>%
  ungroup()

# starters with probs
starters <- rotation_tbl %>%
  left_join(pitcher_probs, by = c("player", "team", "gs", "bf"))

# bullpen pooled by BF-weighted average of probs
bullpen <- pitch_data %>%
  anti_join(rotation_tbl, by = c("player", "team")) %>%
  left_join(pitcher_probs, by = c("player", "team", "gs", "bf")) %>%
  filter(!is.na(probs)) %>%
  group_by(team) %>%
  summarise(
    # weighted average of each prob slot by BF
    probs = list({
      mat <- do.call(rbind, lapply(probs, as.numeric))
      w   <- bf / sum(bf, na.rm = TRUE)
      colSums(mat * w)
    }),
    outcomes = list(c("singles","doubles","triples","home_runs","walks","outs")),
    .groups = "drop"
  )

# If a team is missing bullpen, give league-average neutral probs
if (nrow(bullpen) < length(unique(sim_players$team))) {
  missing_bp <- setdiff(unique(sim_players$team), bullpen$team)
  if (length(missing_bp)) {
    lg_pitch_vec <- pitcher_probs %>%
      summarise(
        singles = sum(map_dbl(probs, 1) * bf) / sum(bf),
        doubles = sum(map_dbl(probs, 2) * bf) / sum(bf),
        triples = sum(map_dbl(probs, 3) * bf) / sum(bf),
        hr      = sum(map_dbl(probs, 4) * bf) / sum(bf),
        bb      = sum(map_dbl(probs, 5) * bf) / sum(bf),
        outs    = sum(map_dbl(probs, 6) * bf) / sum(bf)
      ) %>% as.numeric()
    fallback <- tibble(
      team = missing_bp,
      probs = list(lg_pitch_vec),
      outcomes = list(c("singles","doubles","triples","home_runs","walks","outs"))
    )
    bullpen <- bind_rows(bullpen, fallback)
  }
}

# Helper: return a team's starter i (index) and bullpen probs
get_team_pitching <- function(team, starter_index = 1) {
  s_tbl <- starters %>% filter(team == !!team, !is.na(probs))
  if (nrow(s_tbl) == 0) {
    # fallback to bullpen, or to league neutral
    bp <- bullpen %>% filter(team == !!team)
    if (nrow(bp) == 0) {
      # league neutral from overall pitcher_probs
      lg_pitch_vec <- pitcher_probs %>%
        summarise(
          singles = sum(map_dbl(probs, 1) * bf) / sum(bf),
          doubles = sum(map_dbl(probs, 2) * bf) / sum(bf),
          triples = sum(map_dbl(probs, 3) * bf) / sum(bf),
          hr      = sum(map_dbl(probs, 4) * bf) / sum(bf),
          bb      = sum(map_dbl(probs, 5) * bf) / sum(bf),
          outs    = sum(map_dbl(probs, 6) * bf) / sum(bf)
        ) %>% as.numeric()
      s_row <- tibble(team = team, probs = list(lg_pitch_vec),
                      outcomes = list(c("singles","doubles","triples","home_runs","walks","outs")))
      bp_row <- s_row
    } else {
      s_row <- bp
      bp_row <- bp
    }
  } else {
    idx <- ((starter_index - 1) %% nrow(s_tbl)) + 1
    s_row <- s_tbl[idx, c("team","probs","outcomes")]
    bp_row <- bullpen %>% filter(team == !!team)
    if (nrow(bp_row) == 0) bp_row <- s_row
  }

  list(starter = s_row, bullpen = bp_row)
}

# ------------------------------------------------------------------
# 4) Batter x Pitcher BLENDING (geometric mean, weighted)
# ------------------------------------------------------------------
# p_final(event) ∝ p_batter^w_b * p_pitcher^w_p
batter_weight <- 0.6

blend_probs <- function(batter_probs, pitcher_probs,
                        w_b = batter_weight) {
  w_p <- 1 - w_b
  # geometric mean: exp(w_b*log(b) + w_p*log(p))
  # protect against zeros
  eps <- 1e-12
  b <- pmax(batter_probs,  eps)
  p <- pmax(pitcher_probs, eps)
  log_mix <- w_b * log(b) + w_p * log(p)
  out <- exp(log_mix)
  out / sum(out)
}

simulate_at_bat_pitching <- function(batter_row, p_probs_vec) {
  probs_b <- batter_row$probs[[1]]
  outcomes <- batter_row$outcomes[[1]]
  probs_final <- blend_probs(probs_b, p_probs_vec)
  sample(outcomes, size = 1, prob = probs_final)
}

simulate_inning_pitching <- function(lineup, p_probs_vec) {
  outs <- 0
  score <- 0
  bases <- c(FALSE, FALSE, FALSE)
  batter_index <- 1

  while (outs < 3) {
    batter <- lineup[batter_index, ]
    result <- simulate_at_bat_pitching(batter, p_probs_vec)

    if (result == "outs") {
      outs <- outs + 1
    } else if (result == "walks" || result == "singles") {
      if (bases[3]) score <- score + 1
      bases <- c(TRUE, bases[1], bases[2])
    } else if (result == "doubles") {
      if (bases[3]) score <- score + 1
      if (bases[2]) score <- score + 1
      bases <- c(FALSE, TRUE, bases[1])
    } else if (result == "triples") {
      score <- score + sum(bases)
      bases <- c(FALSE, FALSE, TRUE)
    } else if (result == "home_runs") {
      score <- score + 1 + sum(bases)
      bases <- c(FALSE, FALSE, FALSE)
    }

    batter_index <- (batter_index %% nrow(lineup)) + 1
  }

  score
}

# Starter innings (very rough)
starter_ip_mean <- 6

simulate_game_pitching <- function(lineup1, lineup2, team1, team2,
                                   starter_index_1 = 1, starter_index_2 = 1,
                                   starter_ip = starter_ip_mean) {

  pit1 <- get_team_pitching(team1, starter_index_1)
  pit2 <- get_team_pitching(team2, starter_index_2)

  runs1 <- 0
  runs2 <- 0

  for (inn in 1:9) {
    p1_vec <- if (inn <= starter_ip) pit1$starter$probs[[1]] else pit1$bullpen$probs[[1]]
    p2_vec <- if (inn <= starter_ip) pit2$starter$probs[[1]] else pit2$bullpen$probs[[1]]

    # away bats first (team2)
    runs2 <- runs2 + simulate_inning_pitching(lineup2, p1_vec)
    runs1 <- runs1 + simulate_inning_pitching(lineup1, p2_vec)
  }

  tibble(Team1 = runs1, Team2 = runs2)
}

# ------------------------------------------------------------------
# 5) (Optional) keep your original batter-only engine
# ------------------------------------------------------------------
simulate_at_bat <- function(player_row) {
  sample(player_row$outcomes[[1]], size = 1, prob = player_row$probs[[1]])
}

simulate_inning <- function(lineup) {
  outs <- 0
  score <- 0
  bases <- c(FALSE, FALSE, FALSE)
  batter_index <- 1

  while (outs < 3) {
    player <- lineup[batter_index, ]
    result <- simulate_at_bat(player)

    if (result == "outs") {
      outs <- outs + 1
    } else if (result == "walks" || result == "singles") {
      if (bases[3]) score <- score + 1
      bases <- c(TRUE, bases[1], bases[2])
    } else if (result == "doubles") {
      if (bases[3]) score <- score + 1
      if (bases[2]) score <- score + 1
      bases <- c(FALSE, TRUE, bases[1])
    } else if (result == "triples") {
      score <- score + sum(bases)
      bases <- c(FALSE, FALSE, TRUE)
    } else if (result == "home_runs") {
      score <- score + 1 + sum(bases)
      bases <- c(FALSE, FALSE, FALSE)
    }

    batter_index <- (batter_index %% nrow(lineup)) + 1
  }
  score
}

simulate_game <- function(l1, l2) {
  tibble(Team1 = sum(replicate(9, simulate_inning(l1))),
         Team2 = sum(replicate(9, simulate_inning(l2))))
}

simulate_full_game_log <- function(lineup1, lineup2) {
  team1_name <- unique(lineup1$team)
  team2_name <- unique(lineup2$team)

  if (length(team1_name) == 0) team1_name <- "Team1"
  if (length(team2_name) == 0) team2_name <- "Team2"

  innings <- 9
  score_log <- tibble(
    Inning = as.character(1:innings),
    !!team1_name := integer(innings),
    !!team2_name := integer(innings)
  )

  for (i in 1:innings) {
    score_log[[team1_name]][i] <- simulate_inning(lineup1)
    score_log[[team2_name]][i] <- simulate_inning(lineup2)
  }

  score_log %>%
    add_row(
      Inning = "Total",
      !!team1_name := sum(score_log[[team1_name]]),
      !!team2_name := sum(score_log[[team2_name]])
    )
}
