# run_single_matchup.R

source("setup_simulation.R")

# Choose two teams
team1 <- sim_players %>% filter(team == "NYY") %>% head(9)
team2 <- sim_players %>% filter(team == "LAD") %>% head(9)

# Run 100 simulations
results <- map_dfr(1:100, ~simulate_game(team1, team2))

# View game log for one game
game_log <- simulate_full_game_log(team1, team2)

# Plot: Score differential
results %>%
  ggplot(aes(x = Team1 - Team2)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  labs(title = "100 Game Simulated (NYY vs LAD)",
       x = "NYY - LAD", y = "Games")

# Plot: Win count
results %>%
  mutate(winner = case_when(
    Team1 > Team2 ~ "NYY",
    Team2 > Team1 ~ "LAD",
    TRUE ~ "Tie"
  )) %>%
  count(winner) %>%
  ggplot(aes(x = winner, y = n, fill = winner)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Game Wins by Team", x = "", y = "Wins")

# Display game log
game_log %>%
  gt() %>%
  tab_header(title = "Simulated Game Scoreboard: NYY vs LAD") %>%
  fmt_number(everything(), decimals = 0)
