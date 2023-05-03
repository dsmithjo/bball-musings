# README --------------------

# Second cut
# Analysis, using logistic regression to test the effect of Duncan Robinson's minutes played on the likelihood of the Heat's success.
# Rdata for analysis available here: https://github.com/dsmithjo/bball-musings/blob/main/data/duncan-robinson-game-log2.RData


# LOAD packages --------------------

xfun::pkg_attach("tidyverse", "janitor", "data.table", "gt", "flextable", message = FALSE)


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
         mins_colon = case_when(
           mp ==  "Did Not Dress" ~ "0",
           mp ==  "Did Not Play" ~ "0",
           mp == "Inactive" ~ "0",
           TRUE ~ mp
         ),
         mins_colon2 = mins_colon,
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
         ),
         win = case_when(
           game_outcome == "win" ~ 1,
           TRUE ~ 0
         )) %>%
  separate(mins_colon, c("mins_str", "secs_str"), ":") %>%
  # Warning messages OK to ignore
  # `separate` (1) dropped the "seconds" after the second colon in the `mins_colon`/`mins_colon2` variables, all zeroes, and (2) assigned NA in the string value for seconds (`secs_str`) for any entries with only "0" in the original string value for minutes (`mins_colon`/`mins_colon2`), and therefore no seconds 
  mutate(mins = as.numeric(mins_str),
         secs = as.numeric(secs_str),
         secs = case_when(
           is.na(secs) ~ 0,
           TRUE ~ secs
         ),
         secs_played = ((mins*60) + secs), 
         mins_played = secs_played/60) %>%
  select(season, season_type, game_num, date, duncan_outcome, game_outcome, win, secs_played, mins_played)


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
         mins_colon = case_when(
           mp ==  "Did Not Dress" ~ "0",
           mp ==  "Did Not Play" ~ "0",
           mp == "Inactive" ~ "0",
           TRUE ~ mp
         ),
         mins_colon2 = mins_colon,
         season = case_when(
           date >= "2020-08-18" & date <= "2020-10-11" ~ "2019-20",
           date >= "2021-05-22" & date <= "2021-05-29" ~ "2020-21",
           date >= "2022-04-17" & date <= "2022-05-29" ~ "2021-22",
           date >= "2023-04-16" & date <= "2023-04-26" ~ "2022-23"
         ),
         game_outcome = case_when(
           game_outcome == "W" ~ "win",
           TRUE ~ "loss"
         ),
         win = case_when(
           game_outcome == "win" ~ 1,
           TRUE ~ 0
         )) %>%
  separate(mins_colon, c("mins_str", "secs_str"), ":") %>%
  # Warning messages OK to ignore
  # `separate` (1) dropped the "seconds" after the second colon in the `mins_colon`/`mins_colon2` variables, all zeroes, and (2) assigned NA in the string value for seconds (`secs_str`) for any entries with only "0" in the original string value for minutes (`mins_colon`/`mins_colon2`), and therefore no seconds 
  mutate(mins = as.numeric(mins_str),
         secs = as.numeric(secs_str),
         secs = case_when(
           is.na(secs) ~ 0,
           TRUE ~ secs
         ),
         secs_played = ((mins*60) + secs), 
         mins_played = secs_played/60) %>%
  select(season, season_type, game_num, date, duncan_outcome, game_outcome, win, secs_played, mins_played)

# Counts:
# MIA games since Duncan's first season: 439 
# Regular season: 391
# Post-season: 48


# join data sets
gl_all <- gl_reg %>%
  full_join(gl_post)


# ANALYZE data --------------------

# logistic regression
# predictor = minutes played
model1 <- glm(win~mins_played, data=gl_all, family='binomial')
model1_summ <- summary(model1)  # not shown

# exponentiated results
model1_or <- exp(model1$coef)  # not shown
model1_ci <-exp(confint(model1))  # not shown

# tidy table
(model1_table2 <- cbind("odds ratio" = model1_or, model1_ci) %>%
    as.data.frame() %>%
    rename("lower CI, 95%" = `2.5 %`,
           "upper CI, 95%" = `97.5 %`) %>%
    rownames_to_column("term") %>%
    flextable() 
)   