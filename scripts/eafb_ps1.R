# Empirical Analysis of Firm Behavior
# Problem Set 1
source("functions.R")

data = read_dta("data/eafb_ps1_2021.dta")

glimpse(data)

# Question 1 -------

df = data %>%
  mutate(year = as_factor(year)) %>% 
  group_by(market) %>% 
  mutate(market_value = sum(passengers * price)) %>% 
  group_by(carrier, market) %>% 
  mutate(market_share = passengers * price/market_value * 100) %>% 
  mutate(market_share = round(market_share, digits = 3)) %>% 
  group_by(market) %>% 
  mutate(hhi = sum(market_share^2))

# Checking to make sure there were no problems in calculations

# df %>% 
# filter(market == 108142) %>% 
# view()

# Question 2 -------

df %>% 
  group_by(year) %>% 
  summarize(mean_hhi = mean(hhi))

## From 2016 to 2017, mean HHI across all markets (which we previously defined as origin destination in a given year) increases by 476 points, from 4858 to 5334

## Look at only markets where UA or AA are/were operating. How did HHI and prices change over time ?

df %>% 
  filter(carrier %in% c("UA", "AA")) %>% 
  group_by(carrier, year) %>% 
  summarize(mean_hhi = mean(hhi),
            mean_price = mean(price))

## Now look at a close competitor, Delta Airlines. For Delta it seems mean price didn't change, but HHI (necessarily) increased
df %>% 
  filter(carrier %in% c("DL")) %>% 
  group_by(carrier, year) %>% 
  summarize(mean_hhi = mean(hhi),
            mean_price = mean(price))

## And now all airlines other than United and American

check = df %>% 
  filter(carrier %notin% c("UA", "AA")) %>% 
  group_by(carrier, year) %>% 
  summarize(mean_hhi = mean(hhi),
            mean_price = mean(price)) %>% 
  nest()
 
df %>% 
  filter(carrier %in% c("UA", "AA", "DL")) %>% 
  select(year, market, OD, hhi) %>% 
  view()
