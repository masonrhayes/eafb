# Empirical Analysis of Firm Behavior
# Problem Set 1
source("scripts/functions.R")

data = read_dta("data/eafb_ps1_2021.dta")

glimpse(data)

# Question 1 -------

df = data %>%
  #mutate(year = as_factor(year)) %>% 
  group_by(market) %>% 
  mutate(market_value = sum(passengers * price)) %>%
  mutate(total_passengers = sum(passengers)) %>% 
  group_by(carrier, market) %>% 
  mutate(market_share_sales = passengers * price/market_value * 100) %>% 
  mutate(market_share_sales = round(market_share_sales, digits = 3)) %>% 
  mutate(market_share_passengers = sum(passengers)/total_passengers * 100) %>%
  mutate(market_share_passengers = round(market_share_passengers, digits = 3)) %>% 
  group_by(market) %>% 
  mutate(hhi = sum(market_share_passengers^2))

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
 
df %>% 
  filter(carrier %in% c("UA", "AA", "DL")) %>% 
  select(carrier, year, market, OD, hhi) %>% 
  view()
  
  
## Define dummy for in a UA market or not ------

ua_markets = df %>% 
  filter(carrier %in% c("UA", "AA"))

df = df %>% 
  mutate(ua_market = ifelse(market %in% ua_markets$market, "yes", "no"))

## How did HHI, market value, price, and passengers change over time on average in markets where UA operates vs where it does not ? -----------

## We need to summarize Select only unique markets. Group by year and summarize: ----

table = df %>%
  group_by(carrier, year, ua_market) %>% 
  summarize(mean_hhi = mean(hhi),
            mean_market_value = mean(market_value),
            mean_market_share_passengers = mean(market_share_passengers),
            mean_price = mean(price),
            mean_passengers = mean(passengers),
            mean_total_passengers = mean(total_passengers),
            mean_o1 = mean(owner1),
            mean_o2 = mean(owner2),
            mean_o3 = mean(owner3),
            mean_o4 = mean(owner4),
            mean_o5 = mean(owner5))

model2 = df %>% 
  lm(formula = hhi ~ carrier + year + ua_market + ua_market*year)

summary(model2)

model3 = table %>% 
  filter(between(mean_market_value, 40000, 90000) & between(mean_total_passengers, 200,300)) %>% 
  lm(formula = mean_hhi ~ carrier + year + ua_market + ua_market*year + mean_passengers + mean_market_value)

summary(model3)

## Look at a graph ?
table %>% 
  filter(mean_total_passengers > 200 & mean_market_value > 40000) %>% 
  ggplot(aes(year, mean_hhi)) +
  ft_theme()+
  scale_color_ft()+
  geom_smooth(aes(color = ua_market), se = FALSE) +
  geom_point(aes(color = ua_market)) +
  ylim(3750, 6300)

