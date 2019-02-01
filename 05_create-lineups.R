library(tidyverse)
library(furrr)

# make all possible combos of 5-player sets, get rid of those that "cost" more than $33 for now
twitter_lineup <- read_csv('twitter_lineup_ids.csv')

# let's try eliminating people ####
batters <- read_csv('batter_war_in_lineup.csv') %>% 
  left_join(select(twitter_lineup, position, cost, player_ID), by=c('player_ID')) %>%
  select(name_common, player_ID, position, cost, WAA_tot, WAA_best, WAA_p162, WAA_med, WAA_t7, WAA_def_t7, WAA_off_t7)

# for each position, rank players by each of the metrics to see if anybody could be absolutely removed in favor of a cheaper option
# ... anyone whose best ranking is BELOW their cost is dispensible, since there will be a cheaper better option, given any stat
# yeah, could do this with gather and join, but I don't feel like it right now
batters_ranked <- batters %>% 
  group_by(position) %>% 
  mutate_at(vars(starts_with('WAA')), min_rank) %>%  # min-rank will rank each player at each role from 1=worst to 5=best
  ungroup() %>% 
  mutate(best_rank = pmax(WAA_tot, WAA_best, WAA_p162, WAA_med, WAA_t7, WAA_def_t7, WAA_off_t7))

# flags four $5 players (Chipper, Papi, Ichiro, Roberto Alomar)
# ... david ortiz's best ranking is 2(!)
# ... just removing these guys who'd never get picked would drop the number of potential lineups from 48.8M to 20M
lowrank_players <- batters_ranked %>% filter(best_rank < cost)

# clean up the pitchers too? ####
pitchers <- read_csv('pitcher_war_in_lineup.csv') %>% 
  left_join(select(twitter_lineup, position, cost, player_ID), by=c('player_ID')) %>%
  select(name_common, player_ID, position, cost, WAA_tot, WAA_best, WAA_p162, WAA_med, WAA_t7)

pitchers_ranked <- pitchers %>% 
  group_by(position) %>% 
  mutate_at(vars(starts_with('WAA')), min_rank) %>%  # min-rank will rank each player at each role from 1=worst to 5=best
  ungroup() %>% 
  mutate(best_rank = pmax(WAA_tot, WAA_best, WAA_p162, WAA_med, WAA_t7))

# Greg Maddux and Trevor Hoffman are both outperformed by cheaper players across the board
# removing them and the four bad hitters drops the total lineups to 12.8M
lowrank_pitchers <- pitchers_ranked %>% filter(best_rank < cost)

# define the remaining possible lineups (these should be set in code, but w.e) ####
# unique possible lineups the dumb way ... JK this isn't gonna work
# 1:5 for C, 1B, SS, LF, and CF. 1:4 for SP, 2B, 3B, DH. 1:3 and 5 for RP
p_lineups <- crossing(SP   = 1:4,
                      RP   = c(1:3, 5),
                      C    = 1:5,
                      `1B` = 1:5,
                      `2B` = 1:4,
                      `3B` = 1:4,
                      SS   = 1:5,
                      LF   = 1:5,
                      CF   = 1:5,
                      RF   = 1:4,
                      DH   = 1:4) %>% 
  mutate(unique_lineup = 1e7:(1e7+n()-1)) # unique ID in [10M, 100M)

# convert to long format and remove the options with a total cost > $33 (<9.9M lineups remain)
p_l_long <- p_lineups %>% 
  gather('position', 'cost', -unique_lineup) %>% 
  group_by(unique_lineup) %>% 
  filter(sum(cost) <= 33) %>% 
  ungroup()

# mush together the player values
# ... def_t7 averages 10% of total t7 (tho often enough it's negative), but let's put the same split for pitchers
player_vals <- bind_rows(batters, pitchers) %>% 
  mutate(WAA_def_t7 = if_else(is.na(WAA_def_t7), 0.1 * WAA_t7, WAA_def_t7),
         WAA_off_t7 = if_else(is.na(WAA_off_t7), 0.9 * WAA_t7, WAA_off_t7)) %>% 
  select(position, cost, starts_with('WAA'))

# attach the player vals to the long format and get total value for each lineup by each method
# ugh, this is hitting my vector limit so I'll do it with map (grouped by every million lineups)
p_lineups_vals <- p_l_long %>% 
  group_by(nester = unique_lineup %/% 1e6) %>% 
  nest() %>% 
  transmute(tots = map(data, ~ left_join(., player_vals, by=c('position', 'cost')) %>% 
                         select(-position) %>% 
                         group_by(unique_lineup) %>% 
                         summarise_all(sum))) %>% 
  unnest()

p_lineups_vals %>%
  transmute(cost = as.character(cost),
            winning_pct = (81 + WAA_t7)/162) %>% 
  ggplot(aes(x=cost, y=winning_pct)) +
  geom_violin() +
  geom_point(quantiles=1)


# let's get the best 5 for each 
p_lineups_top10s <- p_lineups_vals %>% 
  gather('stat','val', starts_with('WAA')) %>% 
  group_by(stat) %>% 
  top_n(10, val) %>% 
  ungroup()

# and find out what players are in these
p_lineups_top10s_p <- p_l_long %>% 
  inner_join(rename(p_lineups_top10s,tot_cost = cost), by='unique_lineup') %>% 
  left_join(twitter_lineup, by=c('position', 'cost'))

p_lineups_top10s_p %>%
  select(unique_lineup, tot_cost, stat, val, position, player) %>% 
  spread(position, player) %>% View(  )

p_lineups_top10s_p %>% count(player, position, cost, sort = T) %>% View()


# what position
