pipm_single_season <- c(list.files(pattern = "pipm_2"), list.files(pattern = "pipm_1")) %>% 
  map_df(~read_csv(.)) %>% 
  arrange(season) %>% 
  mutate(Player = case_when(
    Player == "Marcus Williams" & MP > 50 ~"Marcus D. Williams",
    Player == "Tim Hardaway" & season >= 2014 ~ "Tim Hardaway Jr.",
    Player == "Charles Smith" & Age < 31.8 ~ "Charles Cornelius Smith",
    Player == "Peja StojakoviA" ~ "Peja Stojakovic",
    Player == "Walt Lemon" ~ "Walt Lemon Jr.",
    Player == "Luigi Datome" ~ "Gigi Datome",
    Player == "Marcus Williams" & Age != 23.1 ~ "Marcus D. Williams",
    Player == "Clifford Robinson" ~ "Cliff Robinson",
    Player == "Larry Nance" ~ "Larry Nance Jr.",
    Player == "Nene Hilario" ~ "Maybyner Nene",
    Player == "Jakob Poltl" ~ "Jakob Poeltl",
    Player == "Taurean Waller-Prince" ~ "Taurean Prince",
    Player == "Didier Ilunga-Mbenga" ~ "D.J. Mbenga",
    Player == "Clifford Robinson" ~ "Cliff Robinson",
    Player == "Nene" ~ "Maybyner Nene",
    Player == "Kelly Oubre" ~ "Kelly Oubre Jr.",
    Player == "Anfernee Hardaway" ~ "Penny Hardaway",
    Player == "Maurice Taylor" ~ "Mo Taylor",
    Player == "Isaiah Rider" ~ "J.R. Rider",
    Player == "Ronald Murray" ~ "Flip Murray",
    Player == "Isaac Austin" ~ "Ike Austin",
    Player == "CJ McCollum" ~ "C.J. McCollum",
    Player == "Greg Anderson" ~ "Cadillac Anderson",
    Player == "Charles Jones" ~ "Charles A. Jones",
    Player == "Gheorghe Muresan" ~ "George Muresan",
    Player == "John Lucas III" ~ "John Lucas",
    Player == "Matthew Dellavedova" ~ "Matt Dellavedova",
    Player == "Anthony Miller" ~ "Pig Miller",
    Player == "Gary Payton" & Age < 30 ~ "Gary Payton II",
    Player == "Stanislav Medvedenko" ~ "Slava Medvedenko",
    Player == "Glenn Robinson III" ~ "Glenn Robinson",
    Player == "Yi Jianlian" ~ "Jianlian Yi",
    Player == "Raul Neto" ~ "Raulzinho Neto",
    Player == "Roy Rogers" ~ "Roy Rogers Jr.",
    Player == "Predrag Drobnjak" ~ "Peja Drobnjak",
    Player == "Wang Zhizhi" ~ "Zhizhi Wang",
    Player == "Aleksandar Radojevic" ~ "Alek Radojevic",
    Player == "Rafael Araujo" ~ "Babby Araujo",
    Player == "Derrick Jones Jr." ~ "Derrick Jones",
    Player == "Mouhamed Sene" ~ "Saer Sene",
    Player == "James Michael McAdoo" ~ "James McAdoo",
    Player == "Maurice Cheeks" ~ "Mo Cheeks",
    Player == "Ron Grandison" ~ "Ronnie Grandison",
    Player == "Dave Greenwood" ~ "David Greenwood",
    Player == "Cheikh Samb" ~ "Cheickh Samb",
    Player == "Juan Hernangomez" ~ "Juancho Hernangomez",
    Player == "Bruce Brown" ~ "Bruce Brown Jr.",
    Player == "Efthimis Rentzias" ~ "Efthimios Rentzias",
    Player == "Wade Baldwin" ~ "Wade Baldwin IV",
    Player == "Mengke Bateer" ~ "Bateer Mengke",
    Player == "OG Anunoby" ~ "Ogugua Anunoby",
    Player == "Georgios Papagiannis" ~ "George Papagiannis",
    Player == "Dennis Smith Jr." ~ "Dennis Smith",
    Player == "Jose Ortiz" ~ "Jose Rafael Ortiz-Rijos",
    Player == "Hamady N'Diaye" ~ "Hamady Ndiaye",
    Player == "Sviatoslav Mykhailiuk" ~ "Svi Mykhailiuk",
    Player == "Frank Mason III" ~ "Frank Mason",
    Player == "Glen Rice Jr." ~ "Glen Rice",
    Player == "Marcelo Huertas" ~ "Marcelino Huertas",
    Player == "Marcus Vinicius" ~ "Marcus Vinicius",
    Player == "Ognjen Kuzmic" ~ "Ognen Kuzmic",
    Player == "PJ Dozier" ~ "P.J. Dozier",
    Player == "Vitor Luiz Faverani" ~ "Vitor Faverani",
    Player == "William Cunningham" ~ "Will Cunningham",
    Player == "Moritz Wagner" ~ "Moe Wagner",
    TRUE ~ as.character(Player)))

pipm_3_year <- list.files(pattern = "pipm_3yr") %>% 
  map_df(~read_csv(.)) %>% 
  mutate(Age = Age + 1,
         Player = ifelse(Player == "Marcus Williams" & MP > 50, "Marcus D. Williams", Player)) %>% 
  rename(player = Player, age = Age, team_abbrev = Tm, mp = MP, opipm_3yr = OPIPM, 
         dpipm_3yr = DPIPM, pipm_3yr = PIPM, wins_added = `Wins Added`,
         ortg_3yr_onoff = `ORTG On/Off`, drtg_3yr_onoff = `DRTG On/Off`, on_off = `On/Off`) 

pipm_3_year_to_realgm <- pipm_3_year %>% 
  mutate(next_season = end_season + 1,
         player = case_when(
           player == "Tim Hardaway" & end_season >= 2014 ~ "Tim Hardaway Jr.",
           player == "Charles Smith" & age < 31.8 ~ "Charles Cornelius Smith",
           player == "Peja StojakoviA" ~ "Peja Stojakovic",
           player == "Walt Lemon" ~ "Walt Lemon Jr.",
           player == "Luigi Datome" ~ "Gigi Datome",
           player == "Clifford Robinson" ~ "Cliff Robinson",
           player == "Larry Nance" ~ "Larry Nance Jr.",
           player == "Nene Hilario" ~ "Maybyner Nene",
           player == "Jakob Poltl" ~ "Jakob Poeltl",
           player == "Taurean Waller-Prince" ~ "Taurean Prince",
           player == "Didier Ilunga-Mbenga" ~ "D.J. Mbenga",
           player == "Clifford Robinson" ~ "Cliff Robinson",
           player == "Nene" ~ "Maybyner Nene",
           player == "Kelly Oubre" ~ "Kelly Oubre Jr.",
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
           player == "Gary Payton" & age < 30 ~ "Gary Payton II",
           player == "Stanislav Medvedenko" ~ "Slava Medvedenko",
           player == "Glenn Robinson III" ~ "Glenn Robinson",
           player == "Yi Jianlian" ~ "Jianlian Yi",
           player == "Raul Neto" ~ "Raulzinho Neto",
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
           TRUE ~ as.character(player)
         )) 

pipm_3_year %>% 
  group_by(end_season) %>% 
  summarise(mp = mean(mp),
            opipm_3yr = mean(opipm_3yr),
            dpipm_3yr = mean(dpipm_3))

pipm_eval <- pipm_3_year %>% 
  mutate(next_season = end_season + 1) %>% 
  left_join(pipm_single_season, by = c("player" = "Player", "next_season" = "season")) %>% 
  filter(mp > 300) 

opipm_lm <- lm(OPIPM ~ opipm_3yr * mp + age + age**2, data = pipm_eval)
dpipm_lm <- lm(DPIPM ~ dpipm_3yr * mp + age + age**2, data = pipm_eval)

opening_lineup_strength <- opening_night_lineups %>%  
  filter(season > 2000) %>% 
  left_join(pipm_3_year_to_realgm, by = c("Player" = "player", "season" = "next_season")) %>% 
  select(Player, YOS, team, Draft.Status, season, age, end_season, contains("pipm"), mp) 

opening_lineups <- 
  opening_lineup_strength %>% 
  mutate(opipm_proj = predict(opipm_lm, opening_lineup_strength),
         dpipm_proj = predict(dpipm_lm, opening_lineup_strength)) %>% 
  select(Player, team, season, age, opipm_proj, dpipm_proj) %>% 
  arrange(team, season, desc(opipm_proj))

