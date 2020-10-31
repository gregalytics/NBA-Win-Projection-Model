
simulation <- function(games_df = games_2019) {
  games_predicted <- games_df %>% 
    mutate(prob_home = predict(game_xgb_no_elo, games_df, type = "prob")$YES,
           outcome = rbinom(nrow(.), 1, prob_home),
           winner = ifelse(outcome == 1, home_team, away_team))
  
  conference_predicted <- games_predicted %>% 
    group_by(yearSeason, winner) %>% 
    summarise(Wins = n()) %>% 
    arrange(desc(Wins)) %>% 
    mutate(conf = ifelse(winner %in% c("MIL", "TOR", "BOS", "IND", "MIA", "PHI", "BKN", "ORL", "WAS", 
                                       "CHA", "CHI", "NYK", "DET", "ATL", "CLE"), "Eastern Conference", "Western Conference")) %>% 
    arrange(conf) %>% 
    group_by(conf) %>% 
    mutate(conf_rank = 1:n(),
           playoffs_traditional = ifelse(conf_rank <= 8, 1, 0),
           playoffs_prelim = case_when(
             conf_rank <= 7 ~ "YES",
             conf_rank == 8 & Wins - lead(Wins) > 2 ~ "YES",
             conf_rank == 8 & Wins - lead(Wins) <= 2  ~ "PLAYIN1",
             conf_rank == 9 & lag(Wins) - Wins <= 2 ~ "PLAYIN2",
             conf_rank == 9 & lag(Wins) - Wins > 2 ~ "NO",
             conf_rank > 9 ~ "NO"
           ))
  
  play_in_result <- function(conference = "Eastern Conference") {
    
    play_in_conf <- conference_predicted %>% 
      filter(conf_rank %in% c(8, 9), 
             conf == conference) %>% 
      inner_join(projected_net_ratings, by = c("winner" = "teamSlug", "yearSeason" = "season")) 
    
    if(play_in_conf$playoffs_prelim[play_in_conf$conf_rank == 8] == "YES") {
      tibble(play_in_winner = NA, 
             conf = conference)
    }
    
    else {
      play_in <- play_in_conf %>% 
        slice(rep(1:n(), each = 2)) %>% 
        group_by(team) %>% 
        mutate(game = 1:n(),
               home = ifelse(
                 (game %in% c(1, 3) & playoffs_prelim == "PLAYIN1") | game == 2 & playoffs_prelim == "PLAYIN2", 
                 "home", "away")) 
      
      play_in_home <- play_in %>% 
        filter(home == "home") %>% 
        arrange(game) %>% 
        mutate(home_rest = 1, home_b2b_1 = 0) %>% 
        rename(home_off_rating = offensive_rating, home_def_rating = defensive_rating,
               home_net_rating = net_rating, home_team = winner, home_playoffs_prelim = playoffs_prelim)
      
      play_in_away <- play_in %>% 
        filter(home == "away") %>% 
        arrange(game) %>% 
        mutate(away_rest = 1, away_b2b_1 = 0) %>% 
        rename(away_off_rating = offensive_rating, away_def_rating = defensive_rating,
               away_net_rating = net_rating, away_team = winner, away_playoffs_prelim = playoffs_prelim)
      
      
      play_in_merge <- play_in_home %>% 
        inner_join(play_in_away, by = "game") 
      
      play_in_winner <- play_in_merge %>% 
        mutate(prob_home = predict(game_glm, play_in_merge, type = "prob")$YES,
               outcome = rbinom(nrow(.), 1, prob_home),
               game_winner = ifelse(outcome == 1, home_team, away_team),
               game_prelim = ifelse(outcome == 1, home_playoffs_prelim, away_playoffs_prelim)) %>% 
        select(game, outcome, game_winner, game_prelim) %>% 
        mutate(series_outcome = case_when(
          game == 1 & outcome == 1 ~ game_prelim,
          game == 2 & lag(game_prelim == "PLAYIN2") & outcome == 1 ~ game_prelim,
          game == 2 & lag(game_prelim == "PLAYIN1") ~ "PLAYIN1",
          game == 2 & lag(game_prelim == "PLAYIN2") & outcome == 0 ~ "PLAYIN1")) %>% 
        filter(game == 2) %>% 
        select(series_outcome)
      
      tibble(play_in_winner = play_in_winner$series_outcome, 
             conf = conference)
    }
  }
  