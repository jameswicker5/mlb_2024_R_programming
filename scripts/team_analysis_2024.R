## Libraries
library(tidyverse)
library(dplyr)
library(gt)
library(readr)
library(htmltools)

# Load main data
bat_data <- read_csv("data/mlb_bat_2024.csv")
team_colors <- read_csv("data/team_colors.csv")

## Custom Table for Team Stats
team_stats <- bat_data %>%
	filter(!(team %in% c("2TM", "3TM", "4TM"))) %>%
	group_by(team) %>%
	summarize(
		AvgAge = round(mean(age, na.rm = TRUE), 1),
		WAR = sum(war, na.rm = TRUE),
		Hits = sum(hits, na.rm = TRUE),
		HRs = sum(home_runs, na.rm = TRUE),
		Walks	= sum(walks, na.rm = TRUE),
		PAs = sum(plate_appearances, na.rm = TRUE),
		ABs = sum(at_bats, na.rm = TRUE),
		SBs = sum(stolen_bases, na.rm = TRUE),
		BattingAvg = round(sum(hits, na.rm = TRUE) / sum(at_bats, na.rm = TRUE), 3),
    		OBP = round(sum(hits + walks, na.rm = TRUE) / sum(plate_appearances, na.rm = TRUE), 3),
		.groups = 'drop'
	) %>%
	arrange(desc(WAR))

## Create Table for Viewing

# Clean team abbreviations
team_colors <- team_colors %>%
  mutate(abbrev = trimws(abbrev))

# Only keep teams that are present in bat_data
filtered_team_colors <- team_colors %>%
  filter(abbrev %in% bat_data$team)

# Join the color data
team_stats <- team_stats %>%
  left_join(filtered_team_colors, by = c("team" = "abbrev"))

team_stats <- team_stats %>%
  rename_with(~ gsub("(^.)(.*)", "\\U\\1\\L\\2", ., perl = TRUE)) %>%
  rename(
	AvgAge = Avgage,
	WAR = War,
	PAs = Pas,
	ABs = Abs,
	SBs = Sbs,
	Avg = Battingavg,
	OBP = Obp
  )

# Build the table – don't remove primary_color yet
gt_table <- {
  base_tbl <- team_stats %>%
    gt() %>%
    tab_header(title = "2024 Team Batting Summary") %>%
    fmt_number(columns = "AvgAge", decimals = 1) %>%
    fmt_number(columns = c("Avg", "OBP"), decimals = 3) %>%
    fmt_number(
      columns = setdiff(
        names(select_if(team_stats, is.numeric)),
        c("AvgAge", "Avg", "OBP")
      ),
      decimals = 0
    ) %>%
    text_transform(
      locations = cells_body(columns = "Team"),
      fn = function(x) {
        team_colors_lookup <- team_stats %>%
          select(Team, Primary_color) %>%
          distinct()

        mapply(function(team_abbr) {
          color <- team_colors_lookup$Primary_color[team_colors_lookup$Team == team_abbr]
          if (length(color) == 0 || is.na(color)) color <- "#000000"
          htmltools::HTML(paste0('<span style="color:', color, '; font-weight:bold">', team_abbr, '</span>'))
        }, x)
      }
    )

  numeric_cols <- names(select_if(team_stats, is.numeric))
  for (col in numeric_cols) {
    base_tbl <- base_tbl %>%
      tab_style(
        style = list(
          cell_fill(color = "gold"),
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = all_of(col),
          rows = team_stats[[col]] == max(team_stats[[col]], na.rm = TRUE)
        )
      )
  }

  base_tbl %>%
    tab_options(
      table.font.names = "Arial",
      heading.title.font.weight = "bold"
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold", align = "center")
      ),
      locations = cells_column_labels(everything())
    ) %>%
    cols_hide(columns = c("Primary_color", "Secondary_color", "Team.y"))
}

# Print
print(gt_table)


## Save HTML and Team Stats Chart


gtsave(gt_table, "team_stats_2024.html")
gtsave(gt_table, "team_summary_2024.pdf")
write.csv(team_stats, "team_stats_2024.csv", row.names = FALSE)

