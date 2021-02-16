library(shiny)
library(shinydashboard)
library(gt)
library(plotly)
library(paletteer) 

### Functions to use in rendering server

projected_rotation <- function(team_name) {
  team_name <- team_name
  
  minutes_proj %>% 
    mutate(min = minutes_projection / 48,
           offensive_rating = opipm_proj * min,
           defensive_rating = dpipm_proj * min) %>% 
    filter(season == 2019,
           team == team_name) %>% 
    ungroup() %>% 
    select(Player, Min = minutes_projection, OPIPM = opipm_proj, DPIPM = dpipm_proj, PIPM = pipm_proj) %>% 
    gt() %>% 
    tab_header(
      title = paste0(team_name, " Projected Rotation")
    ) %>% 
    fmt_number(
      columns = vars(Min, OPIPM, DPIPM, PIPM),
      decimals = 1
    ) %>%
    cols_align("center") %>%
    data_color(
      columns = everything(),
      colors = teamcolors$secondary[teamcolors$name == team_name]
    ) %>% 
    data_color(
      columns = everything(),
      colors = teamcolors$primary[teamcolors$name == team_name],
      apply_to = "text"
    ) 
}

conf_rank <- function(team_name) {
  
  team <- team_name
  
  rank <-  sims %>% 
    inner_join(team_names, by = c("winner" = "teamSlug")) %>% 
    mutate(`Playoffs?` = factor(case_when(
      playoffs_prelim == "YES" ~ "Yes",
      playoffs_prelim == "NO" ~ "No",
      playoffs_prelim %in% c("PLAYIN1", "PLAYIN2") ~ "Play-In"
    ), levels = c("Yes", "No", "Play-In")),
    `Conference Rank` = factor(conf_rank, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11","12",
                                                     "13", "14", "15"))) %>% 
    filter(team == team_name) %>% 
    ggplot(aes(x = `Conference Rank`, fill = `Playoffs?`)) +
    geom_bar() +
    scale_fill_manual(values = c(teamcolors$primary[teamcolors$name == team_name],
                                 teamcolors$secondary[teamcolors$name == team_name],
                                 teamcolors$tertiary[teamcolors$name == team_name])) +
    labs(y = "Number of Simulations", 
         title = paste0(team, " Simulated 2018-19 Conference Finishes")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggplotly(rank)
}

win_distributions <- function(team_name) {
  team <- team_name
  conf <- sims_with_ratings %>% 
    filter(team == team_name) %>% 
    ungroup() %>% 
    select(conf) %>% 
    distinct()
  
  conference <- conf[[1]]
  
  plot_data <- sims %>% 
    inner_join(conf) %>%
    inner_join(team_names, by = c("winner" = "teamSlug")) 
  
  ggplot(plot_data, aes(x = Wins, y = fct_reorder(winner, Wins, median), color = team, fill = team,
                        label = fct_reorder(winner, Wins, median))) +
    geom_density_ridges() +
    scale_fill_teams(guide = FALSE) +
    scale_color_teams(guide = FALSE) +
    labs(y = "Team", title = paste0(conference, " Projected Win Distributions"))  +
    theme(plot.title = element_text(hjust = 0.5))
  
}

projected_standings <- function(team_name) {
  
  team <- team_name
  conf <- sims_with_ratings %>% 
    filter(team == team_name) %>% 
    ungroup() %>% 
    select(conf) %>% 
    distinct()
  
  conference <- conf[[1]]
  
  sims %>% 
    group_by(winner, conf, yearSeason) %>% 
    summarise(sim_win = mean(Wins),
              sim_loss = 82 - mean(Wins),
              playoffs = mean(playoffs_traditional),
              playoffs_with_playin = mean(playoffs_playin)) %>% 
    arrange(desc(sim_win)) %>% 
    left_join(projected_net_ratings, by = c("winner" = "teamSlug", "yearSeason" = "season")) %>% 
    ungroup() %>% 
    filter(conf == conference) %>% 
    select(team, sim_win, sim_loss, offensive_rating, defensive_rating, playoffs, playoffs_with_playin) %>% 
    gt() %>%
    tab_header(
      title = paste("Projected 2018-19", conference, "Standings and Playoff Odds")
    ) %>% 
    cols_align("center") %>% 
    fmt_percent(
      columns = vars(playoffs, playoffs_with_playin),
      decimals = 1
    ) %>% 
    fmt_number(
      columns = vars(sim_win, sim_loss, offensive_rating, defensive_rating),
      decimals = 1
    ) %>% 
    data_color(
      columns = vars(offensive_rating, defensive_rating),
      colors = scales::col_numeric(
        palette = as.character(paletteer::paletteer_d("ggsci::red_material", n = 5)),
        domain = NULL
      )
    ) %>%
    data_color(
      columns = vars(playoffs, playoffs_with_playin),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    ) %>%
    cols_label(
      team = md("**Team**"),
      sim_win = md("**Wins**"),
      sim_loss = md("**Losses**"),
      offensive_rating = md("**OFF**"),
      defensive_rating = md("**DEF**"),
      playoffs = md("**Playoff %**"),
      playoffs_with_playin = md("**Adj. Playoff %**")
    )
}

### Application

server <- function(input, output) {
  
  output$rotation <- render_gt({
    projected_rotation(input$team)
  })
  
  output$conference_finish <- renderPlotly({
    conf_rank(input$team)
  })
  
  output$win_distribution <- renderPlot({
    win_distributions(input$team)
  })
  
  output$standings <- render_gt({
    projected_standings(input$team)
  })
  
}

ui <-dashboardPage(
  dashboardHeader(title = "2018-19 NBA Win Projections and Playoff Odds", titleWidth = 450),
  dashboardSidebar(selectInput('team', 'Pick a team', unique(team_names$team))),
  dashboardBody(
    fluidRow(
      box(gt_output("rotation")),
      box(gt_output("standings"))
    ),
    fluidRow(
      box(plotlyOutput("conference_finish")),
      box(plotOutput("win_distribution"))
    )))

shinyApp(ui, server)
