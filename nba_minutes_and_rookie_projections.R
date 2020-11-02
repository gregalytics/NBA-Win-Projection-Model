########################################## MINUTES PROJECTIONS    #############################################

## The minutes share of players league-wide has seen noticeable changes at the top. With a bigger emphasis on player rest,
## a star player's share of minutes has gone down from 15.3% of all available team minutes in 2003 to 12.6% of minutes, 
## a drop from 37 minutes to 30 minutes on a 48 minute scale. At large, teams' top 3 options have been on the court less 
## frequently, and the back end of the team's bench is getting more minutes. I think this will be helpful in allocating
## playing time since there is 


## I can use ordinal regression to try and estimate the team's opening night rotation from 1-14. 

minutes_rankings <- per_game_stats %>% 
  mutate(total_min = g * mp) %>% 
  filter(g > 10) %>% 
  arrange(tm, season, desc(total_min)) %>% 
  group_by(tm, season) %>% 
  mutate(minutes_rank = 1:n()) %>% 
  select(player, tm, mp, total_min, minutes_rank) 

team_minutes <- minutes_rankings %>% 
  group_by(tm, season) %>% 
  summarise(team_minutes = sum(total_min)) 

minutes_averages_all <- 
  minutes_rankings %>% 
  inner_join(team_minutes) %>% 
  mutate(minute_share = total_min/team_minutes,
         minutes_rank = ifelse(minutes_rank > 14, 15, minutes_rank)) %>% 
  group_by(season, minutes_rank) %>% 
  summarise(min_share = mean(minute_share)) %>% 
  mutate(min_48 = min_share * 240) %>% 
  arrange(minutes_rank) %>% 
  group_by(minutes_rank) %>% 
  mutate(minutes_projection = (lag(min_48) + lag(min_48, 2) + lag(min_48, 3))/3) 

ggplot(minutes_averages_all, aes(x = season, y = min_share, group = minutes_rank, color = as.factor(minutes_rank))) +
  geom_point() +
  geom_smooth() +
  labs(title = "Minutes Share of Players 1-14 on Roster",
       subtitle = "Top Players have more Maintenance Days, Players 9-14 Have Benefitted",
       x = "Season",
       y = "Minute Share",
       legend = "Minute Ranking") +
  scale_y_continuous(labels = percent) +
  scale_color_discrete(name = "Minutes Rank") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



################# Project rookie minutes, opipm, dpipm

plan(multiprocess) 

drafts <- drafts(draft_years = 1990:2019, nest_data = FALSE, return_message = TRUE) %>% 
  mutate(namePlayer = ifelse(namePlayer == "Marcus Williams" & slugTeam == "NJN", "Marcus D. Williams", namePlayer))

stat_totals <- bref_players_stats(1990:2020, "totals") 

rookies <- minutes_rankings %>% 
  inner_join(team_minutes) %>% 
  mutate(minute_share = total_min/team_minutes,
         minutes_rank = ifelse(minutes_rank > 14, 15, minutes_rank)) %>% 
  filter(tm != "TOT") %>% 
  arrange(player, season) %>% 
  group_by(player) %>% 
  mutate(season_in_league = 1:n()) %>% 
  filter(season_in_league == 1, season != "1990") %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_replace_all(player, "[*,]", "")) %>% 
  left_join(drafts, by = c("player" = "namePlayer")) %>% 
  left_join(pipm_single_season, by = c("player" = "Player", "season")) %>% 
  filter(season < 2020)

# A team like the Pistons had fewer minutes to start Darko Milicic than the Suns ha to play Deandre Ayton

rookies %>% 
  ggplot(aes(x = numberPickOverall, y = minute_share)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Pick Number", y = "Minute Share", title = "Rookie Minute Shares based on Draft Position") +
  theme(plot.title = element_text(hjust = 0.5))

rookies %>% 
  filter(total_min > 500) %>% 
  ggplot(aes(x = numberPickOverall, y = OPIPM)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Pick Number", y = "OPIPM", title = "Rookie OPIPM based on Draft Position") +
  theme(plot.title = element_text(hjust = 0.5))

rookies %>% 
  filter(total_min > 500) %>% 
  ggplot(aes(x = numberPickOverall, y = DPIPM)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Pick Number", y = "DPIPM", title = "Rookie DPIPM based on Draft Position") +
  theme(plot.title = element_text(hjust = 0.5))

rookie_minute_loess <- loess(minute_share ~ numberPickOverall, data = mutate(rookies, 
                                                                             numberPickOverall = ifelse(is.na(numberPickOverall), 61, numberPickOverall)))
rookie_opipm_gam <- gam(OPIPM ~ s(numberPickOverall), data = filter(rookies, total_min > 500))
rookie_dpipm_gam <- gam(DPIPM ~ s(numberPickOverall), data = filter(rookies, total_min > 500))

rookie_values <- 1:61 %>% 
  as_tibble() %>% 
  rename(numberPickOverall = value) %>% 
  mutate(minute_share = predict(rookie_minute_loess, numberPickOverall),
         opipm = predict_gam(rookie_opipm_gam, values = list(numberPickOverall = 1:61))$fit,
         dpipm = predict_gam(rookie_dpipm_gam, values = list(numberPickOverall = 1:61))$fit) 

## Combine opening lineup projections

opening_lineup_projections_initial <- opening_lineups %>% 
  left_join(drafts, by = c("Player" = "namePlayer")) %>% 
  select(Player, team, season, opipm_proj, dpipm_proj, numberPickOverall) %>% 
  filter(!(Player == "Corey Brewer" & numberPickOverall != 7),
         !(Player == "Justin Jackson" & numberPickOverall != 15),
         !(Player == "Dee Brown" & season == 2001 & numberPickOverall != 19),
         !(Player == "Dee Brown" & season > 2002 & numberPickOverall != 46),
         !(Player == "Chris Johnson" & season == 2016 & opipm_proj %in% c(-0.2703148, -0.9082970))) %>% 
  mutate(numberPickOverall = ifelse(is.na(numberPickOverall), 61, numberPickOverall)) %>% 
  left_join(rookie_values)  %>% 
  mutate(minute_share = ifelse(is.na(opipm_proj), minute_share, NA),
         opipm_proj = ifelse(is.na(opipm_proj), opipm, opipm_proj),
         dpipm_proj = ifelse(is.na(dpipm_proj), dpipm, dpipm_proj),
         pipm_proj = opipm_proj + dpipm_proj) %>% 
  select(-opipm, -dpipm) %>% 
  arrange(Player, season) %>% 
  group_by(Player) %>% 
  mutate(season_in_dataset = 1:n())

################################## PROJECT PLAYING TIME ####################################################################

next_season_minutes <- per_game_stats %>% 
  group_by(player, season) %>% 
  mutate(dedupe = 1:n()) %>%  
  filter(dedupe == 1) %>% 
  arrange(player, season) %>% 
  ungroup() %>% 
  mutate(next_season_mp = ifelse(player == lead(player), lead(mp), NA),
         team_change = ifelse(player == lead(player) & tm == lead(tm), 0, 1),
         start_pct = gs/g) %>% 
  filter(!is.na(next_season_mp)) %>% 
  select(player, season, age, tm, team_change, next_season_mp, g, gs, start_pct, efgpercent, pts, ast, trb, mp)

ggplot(next_season_minutes, aes(x = mp, y = next_season_mp)) +
  geom_point() +
  facet_wrap(~team_change) +
  geom_abline()


mp_formula <- formula(next_season_mp ~ mp + age + age**2 + start_pct + pts + trb + ast + team_change)
lm(next_season_mp ~ mp + start_pct + pts + trb + ast + team_change, data = next_season_minutes) %>% 
  summary()

minute_lm <- train(mp_formula, 
                   method = "lm",
                   data = next_season_minutes, 
                   trControl = trainControl(
                     method = "cv"
                   ))

minute_lm$resample

predicted_minutes <- next_season_minutes %>% 
  mutate(minute_pred = predict(minute_lm, next_season_minutes),
         season = season + 1) %>% 
  select(player, season, minute_pred) 


minutes_proj <- opening_lineup_projections_initial %>% 
  left_join(predicted_minutes, by = c("Player" = "player", "season")) %>% 
  mutate(minute_pred = ifelse(season_in_dataset == 1 & season != 2001, minute_share * 240, minute_pred)) %>% 
  arrange(team, season, desc(minute_pred)) %>% 
  group_by(team, season) %>% 
  mutate(minutes_rank = 1:n()) %>% 
  filter(minutes_rank <= 14) %>% 
  left_join(select(minutes_averages_all, season, minutes_rank, minutes_projection)) %>% 
  select(Player, team, season, contains("pipm"), minutes_projection) 

projected_net_ratings <- minutes_proj %>% 
  mutate(min = minutes_projection / 48,
         offensive_rating = opipm_proj * min,
         defensive_rating = dpipm_proj * min) %>% 
  group_by(team, season) %>% 
  summarise(offensive_rating = sum(offensive_rating),
            defensive_rating = sum(defensive_rating),
            net_rating = offensive_rating + defensive_rating) %>% 
  arrange(desc(net_rating)) %>% 
  mutate(teamSlug = case_when(
    team == "Atlanta Hawks" ~ "ATL",
    team == "Boston Celtics" ~ "BOS",
    team == "Brooklyn Nets" ~ "BKN",
    team == "Charlotte Hornets" & season > 2002 ~ "CHA",
    team == "Chicago Bulls" ~ "CHI",
    team == "Cleveland Cavaliers" ~ "CLE",
    team == "Dallas Mavericks" ~ "DAL",
    team == "Denver Nuggets" ~ "DEN",
    team == "Detroit Pistons" ~ "DET",
    team == "Golden State Warriors" ~ "GSW",
    team == "Houston Rockets" ~ "HOU",
    team == "Indiana Pacers" ~ "IND",
    team == "Los Angeles Clippers" ~ "LAC",
    team == "Los Angeles Lakers" ~ "LAL",
    team == "Memphis Grizzlies" ~ "MEM",
    team == "Miami Heat" ~ "MIA",
    team == "Milwaukee Bucks" ~ "MIL",
    team == "Minnesota Timberwolves" ~ "MIN",
    team == "New Orleans Pelicans" | (team == "Charlotte Hornets" & season <= 2002) ~ "NOP",
    team == "New York Knicks" ~ "NYK",
    team == "Oklahoma City Thunder" ~ "OKC",
    team == "Orlando Magic" ~ "ORL",
    team == "Philadelphia Sixers" ~ "PHI",
    team == "Phoenix Suns" ~ "PHX",
    team == "Portland Trail Blazers" ~ "POR",
    team == "Sacramento Kings" ~ "SAC",
    team == "San Antonio Spurs" ~ "SAS",
    team == "Toronto Raptors" ~ "TOR",
    team == "Utah Jazz" ~ "UTA",
    team == "Washington Wizards" ~ "WAS")) 
