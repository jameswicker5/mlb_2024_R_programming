## Load tidyverse for Data Manipulation and Visualization

library(tidyverse)
library(gt)

## Read CSV File

bat_data <- read_csv("C:/Users/james/OneDrive/Desktop/Portfolio/R/data/mlb_bat_2024.csv")

## Quick Glimpse of the Data

glimpse(bat_data)


## Utility Function for Top 10 Plots

plot_top10 <- function(stat_col, label = stat_col) {
	top_players <- bat_data %>%
	filter(!is.na(.data[[stat_col]])) %>%
	arrange(desc(.data[[stat_col]])) %>%
	slice(1:10)


	ggplot(top_players, aes(x= reorder(player, .data[[stat_col]]), y = .data[[stat_col]])) +
		geom_col(fill = "darkgreen") +
		coord_flip() +
		labs(title = paste("Top 10 Players by", label), x ="player", y = label) +
		theme_minimal()
}


## Plots of Batting Statistics

plot_top10("home_runs", "Home Runs")
plot_top10("walks", "Walks")
plot_top10("hits", "Hits")
plot_top10("stolen_bases", "Stolen Bases")



## Team Summary Table

team_stats <- bat_data %>%
	filter(!(team %in% c("2TM", "3TM", "4TM"))) %>%
	group_by(team) %>%
	summarize(
		Avg_Age = round(mean(age, na.rm = TRUE), 1),
		Total_WAR = sum(war, na.rm = TRUE),
		Total_Hits = sum(hits, na.rm = TRUE),
		Total_HRs = sum(home_runs, na.rm = TRUE),
		Total_Walks	= sum(walks, na.rm = TRUE),
		Total_PAs = sum(plate_appearances, na.rm = TRUE),
		Total_ABs = sum(at_bats, na.rm = TRUE),
		Total_SBs = sum(stolen_bases, na.rm = TRUE),
		Batting_Avg = round(sum(hits, na.rm = TRUE) / sum(at_bats, na.rm = TRUE), 3),
    		OBP = round(sum(hits + walks, na.rm = TRUE) / sum(plate_appearances, na.rm = TRUE), 3),
		.groups = 'drop'
	) %>%
	arrange(desc(Total_WAR))

View(team_stats)


## Plot Team Stats
ggplot(team_stats, aes(x = reorder(Team, Total_HRs), y = Total_HRs)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Total Home Runs by Team", x = "Team", y = "HR") +
  theme_minimal()