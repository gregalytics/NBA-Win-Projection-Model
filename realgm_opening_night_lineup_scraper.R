
## Step 1 - Create a team name - RealGM team number - Season data frame for all team seasons 1990-2019

team_names <- game_logs %>% 
  select(nameTeam) %>%  
  distinct() %>% 
  arrange(nameTeam) %>% 
  filter(!nameTeam %in% c("Charlotte Bobcats", "New Orleans/Oklahoma City Hornets", "Seattle SuperSonics",
                          "Vancouver Grizzlies", "Washington Bullets", "Seattle SuperSonics", "New Orleans Hornets", 
                          "New Jersey Nets","LA Clippers")) %>% 
  mutate(team = 1:n(),
         nameTeam = ifelse(nameTeam == "Philadelphia 76ers", "Philadelphia-Sixers", nameTeam),
         team_string = str_replace_all(nameTeam, " ", "-"),
         team = case_when(
           nameTeam == "Brooklyn Nets" ~ 38,
           nameTeam == "Oklahoma City Thunder" ~ 33,
           !nameTeam %in% c("Brooklyn Nets", "Boston Celtics", "Atlanta Hawks", "Oklahoma City Thunder") &
             substr(nameTeam, 1, 1) %in% c("C", "D", "G", "H", "I", "L", "M", "O", "P", "S")  ~ team - 1,
           TRUE ~ as.numeric(team))
  )

team_names <- purrr::map_dfr(seq_len(30), ~team_names) %>% 
  arrange(team) %>% 
  group_by(nameTeam, team) %>% 
  mutate(season = 2020 - 1:n()) %>% 
  filter(!(nameTeam == "Charlotte Hornets" & season %in% c(2003, 2004)),
         !(nameTeam == "New Orleans Pelicans" & season < 2003),
         !(nameTeam == "Memphis Grizzlies" & season < 1996),
         !(nameTeam == "Toronto Raptors" & season < 1996)) 

## Step 2 - Write scraping function, iterate 'safely' over 900 possible seasons

scrape_opening_night_lineups <- function(team_string, team, season) {
  url <- paste0("https://basketball.realgm.com/nba/teams/", team_string, "/", team, "/Rosters/Opening_Day/", season)
  
  url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    .[1] %>% 
    html_table() %>% 
    as.data.frame() %>% 
    mutate(team = team_string, 
           season = season)
}

opening_night_lineups <- 
  pmap_df(list(team_names$team_string, team_names$team, team_names$season), scrape_opening_night_lineups) %>% 
  mutate(team = gsub("-", " ", team, fixed=TRUE),
         Player = str_replace_all(Player, "[,]", ""),
         Player = stri_trans_general(Player, "Latin-ASCII")) %>% #remove dashes needed for URLs
  distinct() %>%  #Ben Wallace is the only duplicated player in data set
  as_tibble() 

#################################### Identify injured players, remove from Opening Night Lineups #####################################
# Remove Injured Players from dataset - i.e. Derrick Rose 2013, 2018 Ben Simmons, 

# ballr functions do not include season as column, so write function that adds in season and iterated over it
get_nba_stats_by_season <- function(season, func = NBAPerGameStatistics) {
  sea <- season
  func(sea) %>% 
    mutate(season = season)
}

per_game_stats <- map_df(1990:2020, get_nba_stats_by_season) 

per_game_stats <- per_game_stats %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_replace_all(player, "[*,]", ""),
         player = case_when(
           player == "Clifford Robinson" ~ "Cliff Robinson",
           player == "Nene" ~ "Maybyner Nene",
           player == "Anfernee Hardaway" ~ "Penny Hardaway",
           player == "Maurice Taylor" ~ "Mo Taylor",
           player == "Isaiah Rider" ~ "J.R. Rider",
           player == "Ronald Murray" ~ "Flip Murray",
           player == "Isaac Austin" ~ "Ike Austin",
           player == "CJ McCollum" ~ "C.J. McCollum",
           player == "Greg Anderson" ~ "Cadillac Anderson",
           player == "Charles Jones" ~ "Charles A. Jones",
           player == "Gheorghe Muresan" ~ "George Muresan",
           player == "John Lucas III" ~ "John Lucas",
           player == "Matthew Dellavedova" ~ "Matt Dellavedova",
           player == "Anthony Miller" ~ "Pig Miller",
           player == "Stanislav Medvedenko" ~ "Slava Medvedenko",
           player == "Glenn Robinson III" ~ "Glenn Robinson",
           link == "/players/s/smithch01.html" ~ "Charles Daniel Smith",
           link == "/players/s/smithch02.html" ~ "Charles Edward Smith IV",
           link == "/players/s/smithch04.html" ~ "Charles Cornelius Smith",
           player == "Yi Jianlian" ~ "Jianlian Yi",
           player == "Raul Neto" ~ "Raulzinho Neto",
           link == "/players/w/willima03.html" ~ "Marcus D. Williams",
           player == "Roy Rogers" ~ "Roy Rogers Jr.",
           player == "Predrag Drobnjak" ~ "Peja Drobnjak",
           player == "Wang Zhizhi" ~ "Zhizhi Wang",
           player == "Aleksandar Radojevic" ~ "Alek Radojevic",
           player == "Rafael Araujo" ~ "Babby Araujo",
           player == "Derrick Jones Jr." ~ "Derrick Jones",
           player == "Mouhamed Sene" ~ "Saer Sene",
           player == "James Michael McAdoo" ~ "James McAdoo",
           player == "Maurice Cheeks" ~ "Mo Cheeks",
           player == "Ron Grandison" ~ "Ronnie Grandison",
           link == "/players/t/tayloje03.html" ~ "Jeffery Taylor",
           link == "/players/j/jonesch03.html" ~ "Charles R. Jones",
           player == "Dave Greenwood" ~ "David Greenwood",
           player == "Cheikh Samb" ~ "Cheickh Samb",
           player == "Juan Hernangomez" ~ "Juancho Hernangomez",
           player == "Bruce Brown" ~ "Bruce Brown Jr.",
           player == "Efthimis Rentzias" ~ "Efthimios Rentzias",
           player == "Wade Baldwin" ~ "Wade Baldwin IV",
           player == "Mengke Bateer" ~ "Bateer Mengke",
           player == "OG Anunoby" ~ "Ogugua Anunoby",
           player == "Georgios Papagiannis" ~ "George Papagiannis",
           player == "Dennis Smith Jr." ~ "Dennis Smith",
           player == "Jose Ortiz" ~ "Jose Rafael Ortiz-Rijos",
           player == "Hamady N'Diaye" ~ "Hamady Ndiaye",
           player == "Sviatoslav Mykhailiuk" ~ "Svi Mykhailiuk",
           player == "Frank Mason III" ~ "Frank Mason",
           player == "Glen Rice Jr." ~ "Glen Rice",
           player == "Marcelo Huertas" ~ "Marcelino Huertas",
           player == "Marcus Vinicius" ~ "Marcus Vinicius",
           player == "Ognjen Kuzmic" ~ "Ognen Kuzmic",
           player == "PJ Dozier" ~ "P.J. Dozier",
           player == "Vitor Luiz Faverani" ~ "Vitor Faverani",
           player == "William Cunningham" ~ "Will Cunningham",
           player == "Moritz Wagner" ~ "Moe Wagner",
           link == "/players/d/dunlemi01.html" ~ "Mike Dunleavy Sr.",
           TRUE ~ as.character(player)
         ))

mp <- per_game_stats %>% 
  select(player, season, mp) 

opening_night_lineups <- opening_night_lineups %>% 
  left_join(mp, by = c("Player" = "player", "season")) %>% 
  filter(!is.na(mp)) %>% 
  select(-mp) %>% 
  distinct() 