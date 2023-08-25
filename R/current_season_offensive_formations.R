library(nflreadr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

# Data-----

current_year <- get_current_season()

pbp <- load_pbp(seasons = current_year)

part <- load_participation(seasons = current_year) |> 
  mutate(nflverse_game_id = str_replace(nflverse_game_id, "_23_", "_22_"))

off_df_raw <- left_join(part, pbp, by = c("nflverse_game_id" = "game_id", "play_id")) |> 
  filter(pass == 1 | rush == 1, !is.na(epa)) |> 
  drop_na(offense_formation)

off_formation1 <- off_df_raw |> 
  select(nflverse_game_id, play_id, season, week, home_team, away_team, home_score, away_score, posteam, defteam, gsis_id = offense_players, offense_personnel, epa, pass) |> 
  separate_rows(offense_personnel, sep = ",") |> 
  mutate(offense_personnel = str_squish(offense_personnel)) |> 
  separate(offense_personnel, into = c("x1", "x2"), sep = " ") |> 
  mutate(x1 = as.numeric(x1)) |> 
  filter(x2 %in% c("RB", "WR", "TE")) |> 
  pivot_wider(names_from = "x2", values_from = "x1") |> 
  janitor::clean_names()

#OPP <- unique(off_formation1$defteam)

off_formation2 <- off_formation1 |> 
  separate_rows(gsis_id, sep = ";")

# Get player/roster info
off_player_positions <- load_players() |> 
  mutate(first_name = gsub('[^[:alnum:] ]', '', first_name)) |> 
  mutate(name = coalesce(
    short_name, 
    paste0(str_sub(first_name, 1, 1), ".", last_name)), 
    .after = "short_name") |> 
  select(short_name = name, gsis_id, position_group, position)

off_formation3 <-  
  left_join(off_formation2, off_player_positions, by = "gsis_id") |> 
  filter(position_group %in% c("RB", "TE", "WR")) |> 
  relocate(short_name, position_group, position, .after = gsis_id) |> 
  mutate(position_group = factor(position_group, c("RB", "TE", "WR"))) |> 
  mutate(formation = paste(rb, te, wr, sep = "-"))

saveRDS(off_formation3, paste0("Data/", current_year, "_offensive_formations.rds"))
