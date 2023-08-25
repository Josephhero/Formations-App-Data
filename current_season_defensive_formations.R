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

def_df_raw <- left_join(part, pbp, by = c("nflverse_game_id" = "game_id", "play_id")) |> 
  filter(pass == 1 | rush == 1, !is.na(epa)) |> 
  drop_na(offense_formation)

def_formation1 <- def_df_raw |> 
  select(nflverse_game_id, play_id, season, week, home_team, away_team, 
         home_score, away_score, posteam, defteam, gsis_id = defense_players, 
         defense_personnel, epa, pass) |> 
  separate_rows(defense_personnel, sep = ",") |> 
  mutate(defense_personnel = str_squish(defense_personnel)) |> 
  separate(defense_personnel, into = c("x1", "x2"), sep = " ") |> 
  mutate(x1 = as.numeric(x1)) |> 
  pivot_wider(names_from = "x2", values_from = "x1") |> 
  janitor::clean_names()

def_formation2 <- def_formation1 |> 
  separate_rows(gsis_id, sep = ";")

# Get player/roster info
def_player_positions <- load_players() |> 
  mutate(first_name = gsub('[^[:alnum:] ]', '', first_name)) |> 
  mutate(name = coalesce(
    short_name, 
    paste0(str_sub(first_name, 1, 1), ".", last_name)), 
    .after = "short_name") |> 
  select(short_name = name, gsis_id, position_group, position)

def_formation3 <-  
  left_join(def_formation2, def_player_positions, by = "gsis_id") |> 
  filter(position_group %in% c("DL", "LB", "DB")) |> 
  relocate(short_name, position_group, position, .after = gsis_id) |> 
  mutate(position_group = factor(position_group, c("DL", "LB", "DB"))) |> 
  mutate(formation = paste(dl, lb, db, sep = "-"))

saveRDS(def_formation3, paste0("data/", current_year, "_defensive_formations.rds"))
