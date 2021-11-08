source("scripts/functions.R")
source("scripts/ps2/load_data_ps2.R")

# Question 1 ------------

head(firm_data)

## Firm Misconduct histogram
firm_histogram = firm_data %>% 
  ggplot() +
  geom_histogram(aes(x = pct_with_misc_records),
                 bins = 50) +
  ft_theme()+
  labs(x = "Percent with Misconduct Records, by Firm", y = "Count")


## Inverse Chi-Squared Distribution for comparison
rinvchisq(100, df = 14) %>% 
  hist(breaks = 50, main = "Inv Chi-Squared Probability Density")

  ## State misconduct histogram

state_histogram = county_data %>% 
  filter(!duplicated(state)) %>% 
  ggplot() +
  geom_histogram(aes(x = pct_misc_in_state),
                 bins = 55) +
  ft_theme()+
  labs(x = "Percent with Misconduct Records, by State", y = "Count",
       title = "Figure 3: State Level Misconduct")+
  xlim(0,0.20)

state_histogram

state_level = county_data %>% 
  filter(!duplicated(state))

rnorm(100, mean(state_level$pct_misc_in_state), sd(state_level$pct_misc_in_state)) %>%
  hist(breaks = 20, main = "Normal Probability Density",
       xlim = c(0,0.2))

# Question 2 --------


county_data %>% 
  filter(countyname != "New York") %>%
  group_by(top_quantile_nbrokers) %>% 
  summarize(mean_nbrokers = mean(nbrokers),
            sd_nbrokers = sd(nbrokers),
            mean_misc = mean(pct_with_misc_records),
            sd_misc = sd(pct_with_misc_records),
            count = n()) %>% 
  view()


# Number of brokers by quantile of misconduct
county_data %>% 
  filter(countyname != "New York") %>% 
  ggplot() +
  facet_wrap(~misc_quantile)+
  geom_histogram(aes(x = sqrt(nbrokers)),
                 bins = 46)+
  ft_theme()+
  labs(title = "Number of Brokers by County", subtitle = "By Quantile of Percent Misconduct")


# Percent misconduct by quantile of nbrokers

county_data %>% 
  filter(countyname != "New York") %>%  
  ggplot() +
  facet_wrap(~nbrokers_quantile)+
  geom_histogram(aes(x = pct_with_misc_records),
                 bins = 50)+
  ft_theme()+
  labs(title = "Percent Misconduct by County", subtitle = "By Quantile of Number of Brokers", x = "Percent Misconduct")


## Misconduct histograms by state -------
## defined in functions.R

### before Idaho

county_data %>% 
  filter(state <= "Idaho") %>% 
  misconduct_by_state

## before Miss, After Idaho
## 
county_data %>% 
  filter(state > "Idaho" & state <= "Mississippi") %>% 
  misconduct_by_state()

## Before OK, after MS
county_data %>% 
  filter(state > "Mississippi" & state <= "Oklahoma") %>% 
  misconduct_by_state()

# the rest
county_data %>% 
  filter(state > "Oklahoma" & state <= "Wyoming") %>% 
  misconduct_by_state()

# Q2 regression -------


q2_model = county_data %>% 
  # filter(countyname != "New York") %>% 
  felm(formula = pct_with_misc_records ~ nbrokers + sq_nbrokers | state)

summary(q2_model)

q2_model %>% 
  stargazer(type = "latex", title = "Q2 Model: State FE" ,
            out = "output/ps2/q2_model.tex",
            label = "q2_model",
            digits = 6,
            dep.var.labels = "Percent Misconduct")

summary(q2_model)


# Question 3 --------
data = county_data %>% 
  left_join(education)


q3_model = data %>%
  # filter(countyname != "New York") %>% 
  felm(formula = pct_with_misc_records ~ nbrokers + sq_nbrokers + pct_hs + pct_some_college | state)

summary(q3_model)


q3_model %>% 
  stargazer(type = "latex", title = "Q3 Model: State FE" ,
            out = "output/ps2/q3_model.tex",
            label = "q3_model",
            digits = 6,
            dep.var.labels = "Percent Misconduct")
