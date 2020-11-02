

################################ Predict game outcomes with calculated ELO and net ratings #################################

# Assumption - every team is going for a win games 1-82 (not the case if clinched playoffs, which can be accounted for,
# but for sake of time I am not worrying about that)
home <- home_games %>% 
  inner_join(projected_net_ratings, by = c("yearSeason" = "season", "home_team" = "teamSlug")) %>% 
  left_join(end_of_season_elos, by = c("yearSeason" = "next_season", "home_team" = "slugTeam")) %>% #Left because Bobcats Inaugural Season
  select(idGame, yearSeason, home_team, elo, home_win, home_rest, home_b2b_1, home_b2b_2, offensive_rating, defensive_rating, net_rating, elo) %>% 
  rename(home_off_rating = offensive_rating, home_last_elo = elo, home_def_rating = defensive_rating, home_net_rating = net_rating) %>% 
  mutate(home_b2b_1 = ifelse(home_b2b_1 == TRUE, 1, 0),
         home_b2b_2 = ifelse(home_b2b_2 == TRUE, 1, 0),
         home_win_fct = ifelse(home_win == 1, "YES", "NO"),
         home_last_elo = ifelse(home_team == "CHA" & yearSeason == 2005, 1305, home_last_elo)) 

away <- away_games %>% 
  inner_join(projected_net_ratings, by = c("yearSeason" = "season", "away_team" = "teamSlug")) %>% 
  left_join(end_of_season_elos, by = c("yearSeason" = "next_season", "away_team" = "slugTeam")) %>% #Left because Bobcats Inaugural Season
  select(idGame, yearSeason, away_team, elo, away_rest, away_b2b_1, away_b2b_2, offensive_rating, defensive_rating, net_rating, elo) %>% 
  rename(away_off_rating = offensive_rating, away_last_elo = elo, away_def_rating = defensive_rating, away_net_rating = net_rating) %>% 
  mutate(away_b2b_1 = ifelse(away_b2b_1 == TRUE, 1, 0),
         away_b2b_2 = ifelse(away_b2b_2 == TRUE, 1, 0),
         away_last_elo = ifelse(away_team == "CHA" & yearSeason == 2005, 1305, away_last_elo))

game_predict <- home %>% 
  inner_join(away) %>% 
  mutate(home_rest_diff = home_rest - away_rest,
         home_rest_adv = case_when(
           home_rest_diff > 0 ~ 1,
           home_rest_diff == 0 ~ 0,
           home_rest_diff < 0 ~ -1
         ),
         home_off_differential = home_off_rating - away_def_rating,
         away_off_differential = away_off_rating - home_def_rating) %>% 
  inner_join(game_distances) %>% 
  mutate(away_dist_travel_sqrt = sqrt(away_dist_travel),
         home_dist_travel_sqrt = sqrt(home_dist_travel))

game_predict_preproc <- preProcess(game_predict, method = c("center", "scale"))
game_predict_scale <- predict(game_predict_preproc, game_predict)


game_model <- game_predict %>% 
  filter(yearSeason < 2019)

games_2019 <- game_predict %>% 
  filter(yearSeason == 2019) # To do simulation on 

game_split <- initial_split(game_model, prop = 3/4, strata = yearSeason)

set.seed(898)
game_training <- training(game_split)
game_testing <- testing(game_split)
game_control <- trainControl(
  summaryFunction = twoClassSummary,
  method = "cv",
  number = 10,
  classProbs = TRUE, 
  verboseIter = TRUE,
  savePredictions = TRUE
)

game_glm <- train(
  home_win_fct ~ home_off_rating + home_def_rating + away_off_rating + away_def_rating,
  data = game_training, 
  method = "glm",
  trControl = game_control
)

game_glm_rest <- train(home_win_fct ~ home_off_rating + home_def_rating + home_b2b_1 + home_rest + 
                         away_off_rating + away_def_rating + away_b2b_1 + away_rest,
                       data = game_training,
                       method = "glm",
                       trControl = game_control)

game_glm_rest2 <- train(home_win_fct ~ home_off_differential + away_off_differential + home_def_rating + home_b2b_1 + home_rest + 
                          away_off_rating + away_def_rating + away_b2b_1 + away_rest,
                        data = game_training,
                        method = "glm",
                        trControl = game_control)

game_glmnet_rest <- train(home_win_fct ~ home_last_elo + home_off_rating + home_def_rating + home_rest + home_b2b_1 + home_b2b_2 +
                            away_last_elo + away_off_rating + away_def_rating + away_rest + away_b2b_1 + away_b2b_2 + home_team +
                            home_dist_travel + away_dist_travel,
                          data = game_training,
                          method = "glmnet",
                          trControl = game_control)


game_glmnet_rest_2 <- train(home_win_fct ~ home_last_elo + home_off_rating + home_def_rating + home_rest +
                              away_last_elo + away_off_rating + away_def_rating + away_rest ,
                            data = game_training,
                            method = "glmnet",
                            trControl = game_control)


game_glm_net_ratings <- train(home_win_fct ~ away_last_elo + away_net_rating + home_net_rating + home_last_elo + 
                                home_rest + away_rest,
                              data = game_training, 
                              method = "glm",
                              trControl = game_control)

game_ranger <- train(home_win_fct ~ home_off_rating + home_def_rating + home_b2b_1 + home_rest + 
                       away_off_rating + away_def_rating + away_b2b_1 + away_rest,
                     data = game_training,
                     method = "ranger",
                     trControl = game_control)


game_gbm <- train(home_win_fct ~ home_off_rating + home_def_rating + home_b2b_1 + home_b2b_2 + home_rest + 
                    away_off_rating + away_def_rating + away_b2b_1 + away_b2b_2 + away_rest,
                  data = game_training,
                  method = "gbm",
                  trControl = game_control)


game_xgb <- train(home_win_fct ~ home_last_elo + home_dist_travel + home_off_rating + home_def_rating + home_b2b_1 + home_b2b_2 + home_rest + 
                    away_last_elo + away_off_rating + away_def_rating + away_b2b_1 + away_b2b_2 + away_rest +
                    away_dist_travel,
                  data = game_training,
                  method = "xgbTree",
                  trControl = game_control)

varImp(game_xgb)

game_xgb_no_elo <- train(home_win_fct ~ home_off_rating + home_def_rating + home_b2b_1 + home_b2b_2 + home_rest + 
                           away_off_rating + away_def_rating + away_b2b_1 + away_b2b_2 + away_rest + home_team +
                           away_dist_travel + home_dist_travel,
                         data = game_training,
                         method = "xgbTree",
                         trControl = game_control)



game_xgb_simpler <- train(home_win_fct ~ home_last_elo + away_last_elo + home_net_rating + away_net_rating + home_def_rating +
                            home_rest_adv + home_dist_travel + away_dist_travel,
                          data = game_training,
                          method = "xgbTree",
                          trControl = game_control)


game_gbm_elos <- train(home_win_fct ~ home_last_elo + home_off_rating + home_def_rating + home_b2b_1 + home_b2b_2 + home_rest + 
                         away_last_elo + away_off_rating + away_def_rating + away_b2b_1 + away_b2b_2 + away_rest,
                       data = game_training,
                       method = "gbm",
                       trControl = game_control)




model_list <- list(
  game_glm = game_glm,
  game_glm_rest = game_glm_rest,
  game_glm_rest2 = game_glm_rest2,
  game_glmnet_rest = game_glmnet_rest,
  game_glmnet_rest_2 = game_glmnet_rest_2,
  game_glm_net_ratings = game_glm_net_ratings,
  game_ranger = game_ranger,
  game_gbm = game_gbm,
  game_xgb = game_xgb,
  game_xgb_no_elo = game_xgb_no_elo,
  game_xgb_simpler = game_xgb_simpler,
  game_gbm_elos = game_gbm_elos
)

resamples <- resamples(model_list)

bwplot(resamples)
dotplot(resamples, metric="ROC")

training_eval <- game_training %>% 
  mutate(xgb_no_elo = predict(game_xgb_no_elo, game_training),
         home_win_fct = as.factor(ifelse(home_win == 1, "YES", "NO")))


testing_eval <- game_testing %>% 
  mutate(xgb_no_elo = predict(game_xgb_no_elo, game_testing),
         home_win_fct = as.factor(ifelse(home_win == 1, "YES", "NO")))

confusionMatrix(training_eval$xgb_no_elo, training_eval$home_win_fct)

confusionMatrix(testing_eval$xgb_no_elo, testing_eval$home_win_fct)


# Can I improve accuracy by accounting for imbalance of home team winning?

game_control_imbalance <- trainControl(
  summaryFunction = twoClassSummary,
  method = "cv",
  number = 10,
  classProbs = TRUE, 
  verboseIter = TRUE,
  sampling = "up",
  savePredictions = TRUE
)

game_xgb_no_elo_upsample <- train(home_win_fct ~ home_off_rating + home_def_rating + home_b2b_1 + home_b2b_2 + home_rest + 
                                    away_off_rating + away_def_rating + away_b2b_1 + away_b2b_2 + away_rest + home_team +
                                    away_dist_travel + home_dist_travel,
                                  data = game_training,
                                  method = "xgbTree",
                                  trControl = trainControl(
                                    summaryFunction = twoClassSummary,
                                    method = "cv",
                                    number = 10,
                                    classProbs = TRUE, 
                                    verboseIter = TRUE,
                                    sampling = "up",
                                    savePredictions = TRUE
                                  ))


game_xgb_no_elo_smote <- train(home_win_fct ~ home_off_rating + home_def_rating + home_b2b_1 + home_b2b_2 + home_rest + 
                                 away_off_rating + away_def_rating + away_b2b_1 + away_b2b_2 + away_rest + home_team +
                                 away_dist_travel + home_dist_travel,
                               data = game_training,
                               method = "xgbTree",
                               trControl = trainControl(
                                 summaryFunction = twoClassSummary,
                                 method = "cv",
                                 number = 10,
                                 classProbs = TRUE, 
                                 verboseIter = TRUE,
                                 sampling = "smote",
                                 savePredictions = TRUE
                               ))

plot(game_xgb_no_elo)

sampling_list <- list(
  orig = game_xgb_no_elo,
  up = game_xgb_no_elo_upsample,
  smote = game_xgb_no_elo_smote
)

resampling <- resamples(sampling_list)
summary(resampling, metric = "ROC")