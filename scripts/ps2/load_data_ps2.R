# County Level Data ----------
county_data = read_csv("data/county_level_stats_2015.csv")%>% 
  group_by(state) %>% 
  mutate(pct_misc_in_state = sum(pct_with_misc_records*nbrokers)/sum(nbrokers)) %>% 
  ungroup() %>%
  mutate(misc_quantile = ifelse(pct_with_misc_records >= quantile(pct_with_misc_records)[4], "top", "middle")) %>% 
  mutate(misc_quantile = ifelse(pct_with_misc_records <= quantile(pct_with_misc_records)[2] & misc_quantile != "top", "bottom", misc_quantile)) %>% 
  mutate(nbrokers_quantile = ifelse(nbrokers >= quantile(nbrokers)[4], "top", "middle")) %>% 
  mutate(nbrokers_quantile = ifelse(nbrokers <= quantile(nbrokers)[2], "bottom", nbrokers_quantile))

view(county_data)

# Firm Level Data --------
firm_data = read_csv("data/firm_level_stats_2015.csv")
