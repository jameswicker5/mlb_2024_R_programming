# setup_simulation.R

library(tidyverse)
library(gt)
library(purrr)

# Load Data
bat_data <- read_csv("C:/Users/james/OneDrive/Desktop/Portfolio/R/data/mlb_bat_2024.csv") %>%
	filter(!team %in% c("2TM", "3TM"))

# Create player-level probabilities
sim_players <- bat_data %>%
  filter(plate_appearances >= 50) %>%
  select(player, team, plate_appearances, at_bats, hits, doubles, triples, home_runs, walks, strikeouts) %>%
  mutate(
    singles = hits - doubles - triples - home_runs,
    outs = at_bats - hits,
    events = singles + doubles + triples + home_runs + walks + outs
  ) %>%
  rowwise() %>%
  mutate(
    probs = list(c(
      singles / events,
      doubles / events,
      triples / events,
      home_runs / events,
      walks / events,
      outs / events
    )),
    outcomes = list(c("singles", "doubles", "triples", "home_runs", "walks", "outs"))
  ) %>%
  ungroup()

# Simulation Functions
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

  return(score)
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

  score_log <- score_log %>%
    add_row(
      Inning = "Total",
      !!team1_name := sum(score_log[[team1_name]]),
      !!team2_name := sum(score_log[[team2_name]])
    )

  return(score_log)
}