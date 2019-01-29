library(tidyverse)

# make all possible combos of 5-player sets, get rid of those that "cost" more than $33 for now
twitter_lineup <- read_csv('twitter_lineup_ids.csv')

# this part isn't working yet, will think about it after lunch
all_combis <- twitter_lineup %>% 
  distinct(position, cost) %>% 
  mutate(rownum = row_number()) %>% # need this to keep mutate from freaking out
  spread(position, cost) %>% 
  select(-rownum) %>% 
  expand(`1B`:`RF`)
