# load packages
library(tidyverse)

# for pitchers, need some consideration about what a single season is

# load pitchers file
pitcher_war_slim <- read_csv('pitcher_war_usecols.csv')

# smush all multi-part player-seasons (from trades, etc.) into a single row per player per year
pitcher_war_season <- pitcher_war_slim %>% 
  group_by(name_common, age, player_ID, year_ID) %>% 
  mutate(stints = 1) %>% # this is basically the same as adding a tally that behaves right with summarize_at
  summarise_at(vars(stints, G:WAR), sum) %>% 
  ungroup()

# full-career totals, bests of the four war/waa categories we care about *calculates peak per category, not overall best season*
pitcher_war_career <- pitcher_war_season %>% 
  group_by(name_common, player_ID) %>% 
  summarise_at(vars(G, GS, IPouts, WAA:WAR), funs(tot=sum, best=max)) %>% 
  ungroup()

# per 200IP (limit this to players with at least 50 career innings to get rid of the really wild rates)
# is this perfect? nah ... gonna be weird for relievers, but this is sorta to offset 
# see https://github.com/tidyverse/dplyr/issues/2113
pitcher_war_p162 <- pitcher_war_season %>% 
  group_by(name_common, player_ID) %>% 
  summarise_at(vars(G:WAR), sum) %>%
  ungroup() %>% 
  filter(IPouts >= 50*3) %>% 
  mutate_at(vars(WAA:WAR), funs(p162=`/`), .$IPouts/600) %>% 
  select(name_common, player_ID, ends_with('p162'))

# median full season (defined as >= 120 IP OR >= 50 games)
pitcher_war_med <- pitcher_war_season %>% 
  filter(G >= 50 | IPouts >= 120*3) %>% 
  group_by(name_common, player_ID) %>% 
  summarise_at(vars(WAA:WAR), funs(med=median)) %>% 
  ungroup()

# mean of top 7 seasons (sorta like what JAWS uses) ... gonna use WAR as ranker here, so that same 7 seasons are used for all cats
# first pass will remove everybody with fewer than 200 career IP
pitcher_war_t7 <- pitcher_war_season %>% 
  group_by(name_common, player_ID) %>% 
  filter(sum(IPouts) >= 200*3) %>% 
  top_n(7, WAR) %>% 
  summarise_at(vars(WAA:WAR), funs(t7=mean))

# combine everything into a single dataset ... all the summary variables have suffixes, so reduce by left_join should work
# only store this for folks with at least 1 full season (no NA in med) and at least 200 games(no NA in t7)  
pitcher_summary <- 
  list(pitcher_war_career, pitcher_war_p162, pitcher_war_med, pitcher_war_t7) %>% 
  reduce(left_join, by=c('name_common', 'player_ID')) %>% 
  drop_na()

# write the result to disk
pitcher_summary %>%
  write_csv('pitcher_war_summary.csv')



