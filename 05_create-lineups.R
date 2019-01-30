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

# gonna take a little thought here
# also got to think about whether I want to run for all 5^11 possible lineups or exclude "too expensive" options first
# also for each metric used, some of the more expensive options are going to be excluded
# ... basically if you score lower than someone cheaper at same position, there's no reason to include you
# ... each exclusion this way removes ~20% of possible options
