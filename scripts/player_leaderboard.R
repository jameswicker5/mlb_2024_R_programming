# Libraries

library(tidyverse)
library(gt)

# Load Dataset

players <- read_csv("/data/non_traded_players_2024.csv")

team_colors <- read_csv("/data/team_colors.csv")

# Leaderboard Creation

leaderboard_war <- players %>%
	select(player, team, league, war, home_runs, batting_average, on_base_percentage) %>%
	arrange(desc(war)) %>%
	slice_head(n = 10) %>%
	left_join(team_colors, by = c("team" = "abbrev"))

# Create display-only version (for gt table)
leaderboard_display <- leaderboard_war %>%
	select(player, team, league, war, home_runs, batting_average, on_base_percentage)

# Build gt table
leaderboard_table <- leaderboard_display %>%
	gt() %>%
	tab_header(
		title = "Top 10 Players by WAR - 2024 Season"
	) %>%
	cols_label(
		player = "Player",
		team = "Team",
		league = "League",
		war = "WAR",
		home_runs = "Home Runs",
		batting_average = "AVG",
		on_base_percentage = "OBP"
	) %>%
	fmt_number(columns = war, decimals = 1) %>%
	fmt_number(columns = c(batting_average, on_base_percentage), decimals = 3)

# Apply styling from full data (still using leaderboard_war)
for (i in 1:nrow(leaderboard_display)) {
	color <- leaderboard_war$primary_color[i]
	leaderboard_table <- leaderboard_table %>%
		tab_style(
			style = cell_text(color = color),
			locations = cells_body(rows = i)
		)
}

leaderboard_table

gtsave(leaderboard_table, "leaderboard_war_2024.pdf")
