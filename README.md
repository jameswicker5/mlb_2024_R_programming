# ⚾ MLB 2024 Batting Analysis (R Project)

## 📌 Overview

This project analyzes **2024 MLB player and team batting stats** using R. It includes data wrangling, team/player leaderboards, visualizations, clustering analysis, and league comparisons. Outputs include formatted tables, PDF reports, and PNG visualizations.

---

## 📁 Directory Structure
mlb_2024_R_programming/
│
├── data/
│ ├── mlb_bat_2024.csv # Raw player stats (all batters)
│ ├── traded_players_2024.csv # Filtered: players with multiple teams
│ ├── non_traded_players_2024.csv # Filtered: players with one team
│ ├── team_colors.csv # MLB team color hex codes
│ └── team_stats_2024.csv # Team-level batting summary
│
├── scripts/
│ ├── batting_analysis.R # Top players in HR, H, BB, SB
│ ├── team_analysis_2024.R # Main team summary & WAR by team
│ ├── league_averages.R # AL vs NL stat comparison
│ ├── player_leaderboard.R # Top 10 players by WAR (PDF)
│ ├── kmeans_cluster.R # K-means + PCA cluster analysis
│ └── traded_players.R # Splits raw data into traded/non-traded
│
├── visuals/
│ ├── Team/
│ │ ├── team_stats_2024.html # Interactive team batting table
│ │ └── team_war.png # Bar chart of team WAR
│ ├── Player/
│ │ └── leaderboard_war_2024.pdf # Top 10 WAR player leaderboard
│ └── Clustering/
│ ├── pca_kmeans_2024.png # Player clusters (PCA plot)
│ └── cluster_feature_barplot.png # Top 3 features per cluster
│
└── README.md # Project summary and instructions

---

## ▶️ How to Run

1. Open in RStudio and set working directory to repo root.
2. Install packages:
```r
install.packages(c("tidyverse", "gt", "ggrepel", "webshot2"))
```
3. Run scripts in scripts/ to generate summaries and outputs.

## 🔍 Highlights

- 📊 Team batting summary (HTML table) with color-coded highlights
- 🏆 PDF leaderboard of Top 10 players by WAR
- 📈 WAR by team visualized in a bar chart using team colors
- 🧠 K-means clustering of player stats with PCA scatter plot
- 🔍 Bar plots showing top distinguishing features per cluster
- ⚾ AL vs NL aggregate stat comparison (totals and averages)
