# Empirical Analysis of Firm Behavior
# Problem Set 1
source("scripts/functions.R")

data = read_dta("data/eafb_ps1_2021.dta")


# Question 1 -------

df = data %>% 
  mutate(merged = ifelse(year == 2016, "no", "yes")) %>% 
  group_by(market) %>% 
  mutate(market_value = sum(passengers * price)) %>%
  mutate(total_passengers = sum(passengers)) %>% 
  group_by(carrier, market) %>% 
  mutate(market_share_sales = passengers * price/market_value * 100) %>% 
  mutate(market_share_sales = round(market_share_sales, digits = 3)) %>% 
  mutate(market_share_passengers = sum(passengers)/total_passengers * 100) %>%
  mutate(market_share_passengers = round(market_share_passengers, digits = 3)) %>% 
  group_by(market) %>% 
  mutate(hhi = sum(market_share_passengers^2)) %>% 
  ungroup() %>% 
  mutate(delta_mhhi = MHHI - hhi)

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
  summarize(mean_hhi = sum(hhi * passengers)/sum(passengers),
            mean_price = sum(price * passengers)/sum(passengers))

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
  filter(carrier == "UA")

aa_markets = df %>% 
  filter(carrier == "AA")

both_markets = df %>% 
  mutate(ua_market = ifelse(market %in% ua_markets$market, 1, 0)) %>% 
  mutate(aa_market = ifelse(market %in% aa_markets$market, 1, 0)) %>% 
  filter(year == 2016) %>% 
  mutate(both_markets = ua_market*aa_market) %>% 
  filter(both_markets == 1)

df = df %>% 
  mutate(merging_markets = ifelse(OD %in% both_markets$OD, "yes", "no"))

## How did HHI, market value, price, and passengers change over time on average in markets where UA operates vs where it does not ? -----------

## We need to summarize Select only unique markets. Group by year and summarize: ----

table = df %>%
  group_by(carrier, year, merging_markets) %>% 
  summarize(mean_price = sum(price*passengers)/sum(passengers),
            mean_hhi = sum(hhi*passengers)/sum(passengers),
            mean_market_value = mean(market_value),
            mean_market_share_passengers = mean(market_share_passengers),
            mean_passengers = mean(passengers),
            mean_total_passengers = mean(total_passengers),
            mean_o1 = mean(owner1),
            mean_o2 = mean(owner2),
            mean_o3 = mean(owner3),
            mean_o4 = mean(owner4),
            mean_o5 = mean(owner5)) %>% 
  arrange(carrier, merging_markets)

model3 = table %>%
  mutate(year = as_factor(year)) %>% 
  filter(between(mean_total_passengers, 150,400)) %>% 
  felm(formula = mean_hhi ~ year + merging_markets + merging_markets*year | carrier)

summary(model3)

## Look at a graph ?
table %>%
  filter(mean_total_passengers %>% between(100,400)) %>% 
  ggplot(aes(year, mean_price)) +
  ft_theme()+
  scale_color_ft()+
  #geom_jitter(aes(color = merging_markets), width = 0.15, height = 0.00) +
  geom_smooth(aes(color = merging_markets), se = FALSE)+
  scale_x_continuous(breaks=2016:2018)

df %>% 
  group_by(year, carrier,merging_markets) %>% 
  summarize(mean_price = round(sum(price* passengers)/sum(passengers), digits = 2)) %>% 
  arrange(carrier, merging_markets) %>% 
  view()



### Q5 --------


model5 = df %>%
  mutate(year = as_factor(year)) %>%
  mutate(OD = as_factor(OD)) %>% 
  felm(formula = log(price) ~ hhi + MHHI + online + roundtrip + year)

summary(model5)

model6 = df %>%
  mutate(year = as_factor(year)) %>%
  mutate(OD = as_factor(OD)) %>% 
  mutate(merging_markets = as_factor(merging_markets)) %>% 
  filter(total_passengers %>% between(200,400) | market_value %>% between(50000,100000)) %>% 
  felm(formula = price ~ merged + merging_markets + merged*merging_markets| carrier)

summary(model6)

model7 = df %>% 
  mutate(year = as_factor(year)) %>%
  felm(formula = price ~ time + merging_markets + merging_markets*time | carrier)

summary(model7)



## Question 4 -------

df = df %>% 
  mutate(owner_merged = ifelse(owner2 > 25 & year == 2018, "yes", "no"))

q4_table = df %>%
  mutate(year = as_factor(year)) %>%
  mutate(OD = as_factor(OD)) %>% 
  mutate(merging_markets = as_factor(merging_markets)) %>% 
  filter(total_passengers %>% between(200,400) | market_value %>% between(50000,100000)) %>% 
  filter(year != 2016) %>% 
  group_by(carrier, year, merging_markets, owner_merged) %>% 
  summarize(mean_price = sum(price*passengers)/sum(passengers),
            mean_hhi = sum(hhi*passengers)/sum(passengers),
            mean_mhhi = sum(MHHI*passengers)/sum(passengers),
            mean_delta_mhhi = sum(delta_mhhi * passengers)/sum(passengers),
            mean_market_value = mean(market_value),
            mean_market_share_passengers = mean(market_share_passengers),
            mean_passengers = mean(passengers),
            mean_total_passengers = mean(total_passengers),
            mean_o1 = mean(owner1),
            mean_o2 = mean(owner2),
            mean_o3 = mean(owner3),
            mean_o4 = mean(owner4),
            mean_o5 = mean(owner5)) %>% 
  arrange(carrier, merging_markets, owner_merged)

view(q4_table)  



df %>% 
  filter(year != 2016) %>% 
  filter(total_passengers %>% between(200,400) | market_value %>% between(50000,100000)) %>% 
  group_by(year, merging_markets, owner_merged) %>% 
  summarize(mean_price = sum(price*passengers)/sum(passengers),
            mean_hhi = sum(hhi*passengers)/sum(passengers),
            mean_mhhi = sum(MHHI*passengers)/sum(passengers),
            mean_delta_mhhi = sum(delta_mhhi * passengers)/sum(passengers)
  ) %>% 
  arrange(merging_markets) # %>% write_csv("output/q4_filters.csv")


q5_model = df %>% 
  mutate(year = as_factor(year)) %>%
  mutate(OD = as_factor(OD)) %>% 
  mutate(merging_markets = as_factor(merging_markets)) %>% 
  mutate(owner_merged = as_factor(owner_merged)) %>% 
  felm(formula = log(price) ~ hhi + delta_mhhi + online + roundtrip + year)

summary(q5_model)

q5_model_fe = df %>% 
  mutate(year = as_factor(year)) %>%
  mutate(OD = as_factor(OD)) %>% 
  mutate(merging_markets = as_factor(merging_markets)) %>% 
  mutate(owner_merged = as_factor(owner_merged)) %>% 
  felm(formula = log(price) ~ hhi + delta_mhhi + online + roundtrip + year | carrier + OD)

summary(q5_model_fe)

## Question 6 ------

q6_model = df %>% 
  mutate(year = as_factor(year)) %>%
  filter(year != 2016) %>% 
  mutate(OD = as_factor(OD)) %>% 
  mutate(merging_markets = as_factor(merging_markets)) %>% 
  mutate(owner_merged = as_factor(owner_merged)) %>% 
  felm(formula = log(price) ~ delta_mhhi + online + roundtrip + owner_merged | OD)

summary(q6_model)

df %>% 
  group_by(owner_merged, year) %>% 
  summarize(mean(price),
            mean(hhi),
            mean(owner2),
            mean(delta_mhhi)) %>% 
  arrange(year, owner_merged) %>% 
  view() # %>% write_csv("output/owner_merger.csv")
