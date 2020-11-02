
#Derived from https://www.rostrum.blog/2018/12/24/nba-travel/ to calculate distances between arenas

game_logs_map <- game_logs(seasons = 1990:2019, result_types = "team", season_types = "Regular Season") %>% 
  mutate(slugTeam = case_when(
    slugTeam == "GOS" ~ "GSW",
    slugTeam == "UTH" ~ "UTA",
    slugTeam == "PHL" ~ "PHI", 
    slugTeam == "SAN" ~ "SAS",
    slugTeam == "CHH" ~ "CHA",
    slugTeam == "NOH" ~ "NOP",
    TRUE ~ as.character(slugTeam)),
    slugOpponent = case_when(
      slugOpponent == "GOS" ~ "GSW",
      slugOpponent == "UTH" ~ "UTA",
      slugOpponent == "PHL" ~ "PHI", 
      slugOpponent == "SAN" ~ "SAS",
      slugOpponent == "CHH" ~ "CHA",
      slugOpponent == "NOH" ~ "NOP",
      TRUE ~ as.character(slugOpponent)),
  ) %>% 
  arrange(slugTeam, slugSeason) %>% 
  mutate(prev_destination = case_when(
    numberGameTeamSeason == 1  ~ slugTeam,
    numberGameTeamSeason > 1 & lag(locationGame) == "A" ~ lag(slugOpponent),
    numberGameTeamSeason > 1 & lag(locationGame) == "H" ~ lag(slugTeam)),
    new_dest = ifelse(locationGame == "A", slugOpponent, slugTeam), 
  ) 


nba_scrape <-
  read_html("https://en.wikipedia.org/wiki/National_Basketball_Association") %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>%
  html_table(fill = TRUE, header = NA) %>%
  .[[1]]  

nba_wrangle <- nba_scrape %>% 
  select(-length(.)) %>%  # remove the last column (NA)
  dplyr::filter(!str_detect(Division, "Conference")) %>% 
  mutate(
    Conference = c(rep("Eastern", 15), rep("Western", 15)),
    Capacity = as.numeric(str_remove(Capacity, ","))
  ) %>% 
  separate(`City, State`, c("City", "State"), sep = ", ") %>% 
  separate(Coordinates, c("Coords1", "Coords2", "Coords3"), " / ") %>% 
  separate(Coords3, c("Latitude", "Longitude"), sep = "; ") %>% 
  separate(Longitude, c("Longitude", "X"), sep = " \\(") %>% 
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(
      str_remove(Longitude, "\\ufeff")  # remove rogue unicode
    )  
  ) %>% 
  select(
    Team, Conference, everything(),
    -Founded, -Joined, -Coords1, -Coords2, -X
  ) %>% 
  as_tibble() %>% 
  add_row(Team = "Seattle SuperSonics", Conference = "Western", Division = "Pacific",
          City ="Seattle", State = "Washington", Arena = "Key Arena", Capacity = 18600, Latitude = 47.6221, Longitude =  -122.3540) %>% 
  add_row(Team = "New Jersey Nets", Conference = "Eastern", Division = "Atlantic",
          City ="East Rutherford", State = "New Jersey", Arena = "Continental Airlines Arena", Capacity = 20049, Latitude = 40.8116, Longitude = -74.0676) %>% 
  add_row(Team = "Vancouver Grizzlies", Conference = "Western", Division = "Pacific",
          City ="Vancouver", State = "British Columbia", Arena = "Rogers Arena", Capacity = 18910, Latitude = 49.2778, Longitude = -123.1088) %>% 
  add_row(Team = "New Orleans/Oklahoma City Hornets", Conference = "Western", Division = "Central",
          City ="Oklahoma City", State = "Oklahoma", Arena = "Chesapeake Energy Arena", Capacity = 18203, Latitude = 35.46333, Longitude = -97.51500)


unique_teams <- game_logs_map %>% 
  select(slugTeam, nameTeam) %>% 
  distinct()

coords <- nba_wrangle %>% 
  left_join(unique_teams, by = c("Team" = "nameTeam")) %>% 
  select(slugTeam, Longitude, Latitude)

combos <- game_logs_map %>% 
  select(prev_destination, new_dest) %>% 
  distinct() %>% 
  left_join(coords, by = c("prev_destination" = "slugTeam")) %>% 
  rename(src_long = Longitude, src_lat = Latitude) %>% 
  left_join(coords, by = c("new_dest" = "slugTeam")) %>% 
  rename(dst_long = Longitude, dst_lat = Latitude) %>% 
  mutate(Distance = distHaversine(cbind(src_long, src_lat),
                                  cbind(dst_long, dst_lat))/1609.344) %>% 
  select(prev_destination, new_dest, Distance)

game_distances <- game_logs_map %>% 
  left_join(combos) %>% 
  select(idGame, locationGame, Distance) %>% 
  spread(key = locationGame, value = Distance) %>% 
  rename(away_dist_travel = A,
         home_dist_travel = H)
