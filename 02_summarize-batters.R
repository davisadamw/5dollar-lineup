# load packages
library(tidyverse)

# for batters this should all be pretty straightforward ~ season definable just with games (ignoring PA for now)

# load batters file
batter_war_slim <- read_csv('batter_war_usecols.csv')

# smush all multi-part player-seasons (from trades, etc.) into a single row per player per year
batter_war_season <- batter_war_slim %>% 
  group_by(name_common, age, player_ID, year_ID) %>% 
  mutate(stints = 1) %>% # this is basically the same as adding a tally that behaves right with summarize_at
  summarise_at(vars(stints, G:WAR), sum) %>% 
  ungroup()

# full-career totals, bests of the four war/waa categories we care about *calculates peak per category, not overall best season*
batter_war_career <- batter_war_season %>% 
  group_by(name_common, player_ID) %>% 
  summarise_at(vars(G, WAA:WAR), funs(tot=sum, best=max)) %>% 
  ungroup()

# per 162 (limit this to players with at least 40 games to get rid of the really wild rates)
# see https://github.com/tidyverse/dplyr/issues/2113
batter_war_p162 <- batter_war_season %>% 
  group_by(name_common, player_ID) %>% 
  summarise_at(vars(G:WAR), sum) %>%
  ungroup() %>% 
  filter(G >= 40) %>% 
  mutate_at(vars(WAA:WAR), funs(p162=`/`), .$G/162) %>% 
  select(name_common, player_ID, ends_with('p162'))

# median full season (defined as >=120 120 games)
batter_war_med <- batter_war_season %>% 
  filter(G >= 120) %>% 
  group_by(name_common, player_ID) %>% 
  summarise_at(vars(WAA:WAR), funs(med=median)) %>% 
  ungroup()

# mean of top 7 seasons (sorta like what JAWS uses) ... gonna use WAR as ranker here, so that same 7 seasons are used for all cats
# first pass will remove everybody with fewer than 200 career games
batter_war_t7 <- batter_war_season %>% 
  group_by(name_common, player_ID) %>% 
  filter(sum(G) >= 200) %>% 
  top_n(7, WAR) %>% 
  summarise_at(vars(WAA:WAR), funs(t7=mean))

# combine everything into a single dataset ... all the summary variables have suffixes, so reduce by left_join should work
# only store this for folks with at least 1 full season (no NA in med) and at least 200 games(no NA in t7)  
batter_summary <- 
  list(batter_war_career, batter_war_p162, batter_war_med, batter_war_t7) %>% 
  reduce(left_join, by=c('name_common', 'player_ID')) %>% 
  drop_na()

# write the result to disk
batter_summary %>%
  write_csv('batter_war_summary.csv')



