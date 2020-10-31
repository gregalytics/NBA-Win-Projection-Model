
teams <- home_games %>% 
  select(home_team) %>% 
  rename(team = home_team) %>% 
  distinct() %>% 
  mutate(elo = 1500)

elo_table <- elo.run(score(home_points, away_points) ~ home_team + away_team,
                     data = matchups, k = 20) %>% 
  as.data.frame() %>% 
  select(elo.A, elo.B)

elos_by_game <- bind_cols(matchups, elo_table) %>% 
  select(idGame, elo.A, elo.B) %>% 
  rename(H = elo.A, 
         A = elo.B) %>% 
  gather(key = "locationGame", value = "elo", -idGame) 

games_with_elos <- games %>% 
  inner_join(elos_by_game) 

end_of_season_elos <- games_with_elos %>% 
  filter(numberGameTeamSeason == 82 & !slugSeason %in% c("1998-99", "2011-12") |
           numberGameTeamSeason == 50 & slugSeason == "1998-99" |
           numberGameTeamSeason == 66 & slugSeason == "2011-12") %>% 
  select(slugSeason, yearSeason, slugTeam, elo) %>% 
  mutate(next_season = yearSeason + 1)


end_of_season_elos %>% 
  filter(slugTeam == "MIA") %>% 
  ggplot(aes(x = slugSeason, y = elo)) +
  geom_point()