# load war data from baseball-reference

# load the packages
library(tidyverse)

# load comma delimited data from the web
batter_war_all <- read_csv('https://www.baseball-reference.com/data/war_daily_bat.txt')
pitcher_war_all <- read_csv('https://www.baseball-reference.com/data/war_daily_pitch.txt')

# pull out the variables we care about 
# convert everything but name, player_ID, and team_ID to numeric (will mess with seasons with no PA/IP) ... drop the rows with NAs
batter_war_slim <- batter_war_all %>% 
  select(name_common, age, player_ID, year_ID, team_ID, G, WAA, WAA_off, WAA_def, WAR) %>% 
  mutate_at(vars(WAA:WAR), as.numeric) %>% 
  drop_na()

pitcher_war_slim <- pitcher_war_all %>% 
  select(name_common, age, player_ID, year_ID, team_ID, G, GS, IPouts, WAA, WAR) %>% 
  mutate_at(vars(WAA, WAR), as.numeric) %>% 
  drop_na()

# stash the results for later
pitcher_war_slim %>% write_csv('pitcher_war_usecols.csv')
batter_war_slim %>% write_csv('batter_war_usecols.csv')
