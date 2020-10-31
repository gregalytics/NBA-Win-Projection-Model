library(nbastatR)
library(tidyverse)
library(tidylog)
library(ballr)
library(elo)
library(rvest)
library(purrr)
library(stringi)
library(brms)
library(caret)
library(tidymodels)
library(tidymv)
library(mgcv)
library(ggridges)
library(future)
library(teamcolors)
library(ggcorrplot)
library(osrm)
library(geosphere)

game_logs <- game_logs(seasons = 1990:2019, result_types = "team", season_types = "Regular Season") %>% 
  mutate(possessions = fgaTeam + (0.44 * ftaTeam) + tovTeam - orebTeam,
         countDaysRestTeam = ifelse(countDaysRestTeam > 5, 5, countDaysRestTeam), #Trim Rest Days
         slugTeam = case_when(
           slugTeam == "NJN" ~ "BKN",
           slugTeam == "SEA" ~ "OKC",
           slugTeam == "VAN" ~ "MEM",
           slugTeam == "GOS" ~ "GSW",
           slugTeam == "UTH" ~ "UTA",
           slugTeam == "PHL" ~ "PHI", 
           slugTeam %in% c("NOK", "NOH", "CHH") ~ "NOP",
           slugTeam == "SAN" ~ "SAS",
           TRUE ~ as.character(slugTeam))) 

games <- game_logs %>% 
  select(slugSeason, idGame, yearSeason, numberGameTeamSeason, dateGame, slugTeam,  isWin, ptsTeam, possessions,
         isB2BFirst, isB2BSecond, locationGame, countDaysRestTeam) 

home_games <- games %>% 
  filter(locationGame == "H") %>% 
  mutate(home_win = ifelse(isWin == TRUE, 1, 0)) %>% 
  rename(home_team = slugTeam,
         home_points = ptsTeam,
         home_possessions = possessions,
         home_b2b_1 = isB2BFirst,
         home_b2b_2 = isB2BSecond,
         home_rest = countDaysRestTeam,
         home_game_no = numberGameTeamSeason) %>% 
  select(-locationGame, -isWin)

away_games <- games %>% 
  filter(locationGame == "A") %>% 
  rename(away_team = slugTeam,
         away_points = ptsTeam,
         away_possessions = possessions,
         away_b2b_1 = isB2BFirst,
         away_b2b_2 = isB2BSecond,
         away_rest = countDaysRestTeam,
         away_game_no = numberGameTeamSeason) %>% 
  select(-isWin, - locationGame, -dateGame) #Only need Home Win for prediction

matchups <- home_games %>% 
  inner_join(away_games) 
