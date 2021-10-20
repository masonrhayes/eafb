# Some summary stats of market shares and hhi. Change grouping to see more or less details

ua_table = df %>% 
  filter(carrier %in% c("UA", "AA")) %>% 
  group_by(year,merging_markets) %>% 
  select(market_share_passengers, hhi, price) %>% 
  summarize(mean(market_share_passengers),
            sd(market_share_passengers),
            median(market_share_passengers),
            mean(hhi),
            sd(hhi),
            median(hhi)) %>% 
  arrange(merging_markets)%>% 
  view()

all_table = df %>%  
  group_by(year,merging_markets) %>% 
  select(market_share_passengers, hhi, price) %>% 
  summarize(mean_market_share = round(mean(market_share_passengers), digits = 2),
            mean_price = round(mean(price), digits = 2),
            mean_hhi = round(mean(hhi), digits = 0)) %>% 
  arrange(merging_markets)%>% 
  view()

all_table %>% 
  write_csv("output/all_table.csv")


filtered_table = df %>% 
  filter(total_passengers > 200 | market_value > 76000) %>% 
  group_by(year,merging_markets) %>% 
  select(market_share_passengers, hhi, price) %>% 
  summarize(mean_market_share = round(mean(market_share_passengers), digits = 2),
            mean_price = round(mean(price), digits = 2),
            mean_hhi = round(mean(hhi), digits = 0)) %>% 
  arrange(merging_markets)%>% 
  view()

filtered_table %>% 
  write_csv("output/filtered_table.csv")

# Check united stats

t = df %>% 
  filter(carrier %in% c("UA", "AA")) %>% 
  mutate(carrier = ifelse(carrier == "AA", "UA", "UA")) %>% 
  group_by(carrier) %>% 
  summarize(mean(total_passengers),
            median(total_passengers),
            min(total_passengers),
            mean(market_value),
            median(market_value),
            min(market_value)) %>% 
  view()
