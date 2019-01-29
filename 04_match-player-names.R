# load packages
library(tidyverse)

# note that i did this to preformat the dataset (sorta ... also had to add the position sorting instructions)
# crossing(pos=c('DH','SP','RP','C','1B','2B','SS','3B','LF','CF','RF'), cost=1:5) %>% clipr::write_clip()

# load the players and the two summary datasets to make sure names match / get player_ID for the people in the lineup
twitter_lineup <- read_csv('twitter_lineup.csv')
batter_summary <- read_csv('batter_war_summary.csv')
pitcher_summary <- read_csv('pitcher_war_summary.csv')

# see if anybody in your twitter lineup is missing or duplicated

# first for batters
batter_name_check <- twitter_lineup %>% 
  filter(! position %in% c('SP','RP')) %>% 
  left_join(select(batter_summary, name_common, player_ID), by=c('player'='name_common'))
# ... note the length here is 1 longer than it should be

# missings? Nope
batter_name_check %>% filter(is.na(player_ID))

# duplicates? Frank Thomas!
batter_name_check %>%
  group_by(player) %>% filter(n() > 1)

# use baseball-reference to see which one's the Big Hurt
# https://www.baseball-reference.com/players/t/thomafr04.shtml <- that's our dude
batter_name_correct <- batter_name_check %>% filter(player_ID != 'thomafr03')

# same process for pitchers
pitcher_name_check <- twitter_lineup %>% 
  filter(position %in% c('SP','RP')) %>% 
  left_join(select(pitcher_summary, name_common, player_ID), by=c('player'='name_common'))

# right number, so we're probably good here ... no NA's so we're definitely good
pitcher_name_check %>% filter(is.na(player_ID))

# combine the two datasets and save to disk
updated_twitter_lineup <- bind_rows(pitcher_name_check, batter_name_correct)
updated_twitter_lineup %>% write_csv('twitter_lineup_ids.csv')
