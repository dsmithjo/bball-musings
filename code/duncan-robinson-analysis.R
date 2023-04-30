# README --------------------

# Last updated: 2023-04-29  

# Analysis, using chi-square test, of whether Duncan Robinson playing is related to the Miami Heat's wins and losses.
# Rdata for analysis available here: https://github.com/dsmithjo/bball-musings/blob/main/data/duncan-robinson-game-log.RData


# LOAD packages --------------------

xfun::pkg_attach("tidyverse", "janitor", "data.table", message = FALSE)


# IMPORT and TIDY data --------------------

# data: Duncan Robinson's game logs, regular season and post-season
# data source: CSVs via GitHub
# abbreviated data cleaning steps shown

# import regular season and tidy data
gl_reg <- fread('https://raw.githubusercontent.com/dsmithjo/bball-musings/main/data/duncan-robinson-game-logs-reg-season-to-20230429.csv') %>%
  clean_names() %>%
  separate(v8, c("game_outcome", "pt_diff"), " \\(") %>%
  rename(game_num = "rk") %>%
  mutate(season_type = "regular",
         duncan_outcome = case_when(
           mp ==  "Did Not Dress" ~ "did not play",
           mp ==  "Did Not Play" ~ "did not play",
           mp == "Inactive" ~ "did not play",
           TRUE ~ "played"
         ),
         season = case_when(
           date >= "2018-10-17" & date <= "2019-04-10" ~ "2018-19",
           date >= "2019-10-23" & date <= "2020-08-14" ~ "2019-20",
           date >= "2020-12-23" & date <= "2021-05-16" ~ "2020-21",
           date >= "2021-10-21" & date <= "2022-04-10" ~ "2021-22",
           date >= "2022-10-19" & date <= "2023-04-09" ~ "2022-23"
         ),
         game_outcome = case_when(
           game_outcome == "W" ~ "win",
           TRUE ~ "loss"
         )) %>%
  select(season, season_type, game_num, date, duncan_outcome, game_outcome)

# import post-season and tidy data
gl_post <- fread('https://raw.githubusercontent.com/dsmithjo/bball-musings/main/data/duncan-robinson-game-logs-post-season-to-20230429.csv') %>%
  clean_names() %>%
  separate(v9, c("game_outcome", "pt_diff"), " \\(") %>%
  rename(game_num = g,
         date = x2020_playoffs) %>%
  mutate(season_type = "post",
         duncan_outcome = case_when(
           mp ==  "Did Not Play" ~ "did not play",
           TRUE ~ "played"
         ),
         season = case_when(
           date >= "2020-08-18" & date <= "2020-10-11" ~ "2019-20",
           date >= "2021-05-22" & date <= "2021-05-29" ~ "2020-21",
           date >= "2022-04-17" & date <= "2022-05-29" ~ "2021-22",
           date >= "2023-04-16" & date <= "2023-04-26" ~ "2022-23"
         ),
         game_outcome = case_when(
           game_outcome == "W" ~ "win",
           TRUE ~ "loss"
         )) %>%
  select(season, season_type, game_num, date, duncan_outcome, game_outcome)

# Counts:
# MIA games since Duncan's first season: 439 
# Regular season: 391
# Post-season: 48


# join data sets
gl_all <- gl_reg %>%
  full_join(gl_post)


# ANALYZE data --------------------

# crosstab
(tab <- gl_all %>%
   tabyl(duncan_outcome, game_outcome))

# chi-square test
(chi <- chisq.test(tab, correct=FALSE))
