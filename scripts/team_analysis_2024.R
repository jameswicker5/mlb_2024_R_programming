## Libraries
library(tidyverse)
library(dplyr)
library(gt)
library(readr)
library(htmltools)
library(webshot2)

# Load main data
bat_data <- read_csv("C:/Users/james/OneDrive/Desktop/Portfolio/R/data/mlb_bat_2024.csv")
team_colors <- read_csv("C:/Users/james/OneDrive/Desktop/Portfolio/R/data/team_colors.csv")



## Custom Table for Team Stats
team_stats <- bat_data %>%
  filter(!(team %in% c("2TM", "3TM", "4TM"))) %>%
  group_by(team) %>%
  summarize(
    AvgAge = round(mean(age, na.rm = TRUE), 1),
    WAR = sum(war, na.rm = TRUE),
    PAs = sum(plate_appearances, na.rm = TRUE),
    ABs = sum(at_bats, na.rm = TRUE),
    Runs = sum(runs, na.rm = TRUE),
    Hits = sum(hits, na.rm = TRUE),
    Doubles = sum(doubles, na.rm = TRUE),
    Triples = sum(triples, na.rm = TRUE),
    HRs = sum(home_runs, na.rm = TRUE),
    RBIs = sum(runs_batted_in, na.rm = TRUE),
    SBs = sum(stolen_bases, na.rm = TRUE),
    CS = sum(caught_stealing, na.rm = TRUE),
    Walks = sum(walks, na.rm = TRUE),
    SOs = sum(strikeouts, na.rm = TRUE),
    GIDP = sum(double_plays, na.rm = TRUE),
    TotalBases = sum(total_bases, na.rm = TRUE),
    BattingAvg = round(Hits / ABs, 3),
    OBP = round((Hits + Walks) / PAs, 3),
    SLG = round(TotalBases / ABs, 3),
    OPS = round(OBP + SLG, 3),
    Team_Rbat = sum(runs_above_base_average, na.rm = TRUE),
    HBPs = sum(hit_by_puitch, na.rm = TRUE),
    SacBunts = sum(sacrifice_bunts, na.rm = TRUE),
    SacFlies = sum(sacrifice_flies, na.rm = TRUE),
    IBB = sum(intentional_walks, na.rm = TRUE),
    Awards = sum(!is.na(awards) & awards != ""),
    .groups = 'drop'
  ) %>%
  arrange(desc(WAR))

## League Averages for Advanced Metrics
league_totals <- bat_data %>%
  filter(!(team %in% c("2TM", "3TM", "4TM"))) %>%
  summarize(
    lg_hits = sum(hits, na.rm = TRUE),
    lg_walks = sum(walks, na.rm = TRUE),
    lg_PAs = sum(plate_appearances, na.rm = TRUE),
    lg_ABs = sum(at_bats, na.rm = TRUE),
    lg_doubles = sum(doubles, na.rm = TRUE),
    lg_triples = sum(triples, na.rm = TRUE),
    lg_HRs = sum(home_runs, na.rm = TRUE),
    total_rbat = sum(runs_above_base_average, na.rm = TRUE),
    league_avg_rbat_per_PA = total_rbat / lg_PAs
  ) %>%
  mutate(
    lg_OBP = (lg_hits + lg_walks) / lg_PAs,
    lg_singles = lg_hits - (lg_doubles + lg_triples + lg_HRs),
    lg_total_bases = lg_singles + (2 * lg_doubles) + (3 * lg_triples) + (4 * lg_HRs),
    lg_SLG = lg_total_bases / lg_ABs
  )

# Extract league averages
lg_OBP <- league_totals$lg_OBP
lg_SLG <- league_totals$lg_SLG
lg_rbat_per_PA <- league_totals$league_avg_rbat_per_PA

# Final advanced metrics added to team_stats
team_stats <- team_stats %>%
  mutate(
    OPSPlus = round(100 * ((OBP / lg_OBP) + (SLG / lg_SLG) - 1), 1),
    RbatPA = round(Team_Rbat / PAs, 4),
    RbatPlus = round(100 * (RbatPA / lg_rbat_per_PA), 1)
  )

# Clean team abbreviations
team_colors <- team_colors %>%
  mutate(abbrev = trimws(abbrev))

# Only keep teams that are present in bat_data
filtered_team_colors <- team_colors %>%
  filter(abbrev %in% bat_data$team)

# Join the color data
team_stats <- team_stats %>%
  left_join(filtered_team_colors, by = c("team" = "abbrev"))


# Build the table – don't remove primary_color yet
gt_table <- {
  base_tbl <- team_stats %>%
    gt() %>%
    tab_header(title = "2024 Team Batting Summary") %>%
    fmt_number(columns = c("RbatPlus", "AvgAge", "WAR"), decimals = 1) %>%
    fmt_number(columns = "RbatPA", decimals = 4) %>%
    fmt_number(columns = c("BattingAvg", "OBP", "SLG", "OPS", "Team_Rbat"), decimals = 3) %>%
    fmt_number(
      columns = setdiff(
        names(select_if(team_stats, is.numeric)),
        c("BattingAvg", "OBP", "SLG", "OPS", "Team_Rbat")
      ),
      decimals = 0
    ) %>%
    text_transform(
      locations = cells_body(columns = "team"),
      fn = function(x) {
        team_colors_lookup <- team_stats %>%
          select(team, primary_color) %>%
          distinct()

        mapply(function(team_abbr) {
          color <- team_colors_lookup$primary_color[team_colors_lookup$team == team_abbr]
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
    cols_label(team = "Team") %>%
    cols_hide(columns = c("RbatPA", "primary_color", "secondary_color", "team.y"))
}

# Print
print(gt_table)

team_stats_cleaned <- team_stats %>%
	select(-c(team.y, primary_color, secondary_color, RbatPA))
## Save HTML and Team Stats Chart


gtsave(gt_table, "team_stats_2024.html")
write.csv(team_stats_cleaned, "team_stats_2024.csv", row.names = FALSE)

