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
  ggplot(aes(x = pct_with_misc_records)) +
  facet_wrap(~nbrokers_quantile)+
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
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

q3_model = data %>%
  # filter(countyname != "New York") %>% 
  felm(formula = pct_with_misc_records ~ nbrokers + sq_nbrokers + pct_less_than_hs + pct_bachelors | state)

summary(q3_model)


q3_model %>% 
  stargazer(type = "latex", title = "Q3 Model: State FE" ,
            out = "output/ps2/q3_model.tex",
            label = "q3_model",
            digits = 6,
            dep.var.labels = "Percent Misconduct")

# Histogram of educational data, pct < HS
data %>% 
  filter(pct_less_than_hs > 0) %>% 
  filter(state <= "Maine") %>% 
  education_by_state()

## After Maine, until NM
data %>% 
  filter(pct_less_than_hs > 0) %>% 
  filter(state > "Maine" & state <= "New Mexico") %>% 
  education_by_state()

# After NM, until TN
data %>% 
  filter(pct_less_than_hs > 0) %>% 
  filter(state > "New Mexico" & state <= "Tennessee") %>% 
  education_by_state()

data %>% 
  filter(pct_less_than_hs > 0) %>% 
  filter(state > "Tennessee" & state <= "Wyoming") %>% 
  education_by_state()


## facet wrap by quantiles of misconduct or brokers

data %>% 
  ggplot(aes(x = pct_bachelors))+
  facet_wrap(~misc_quantile)+
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  ft_theme()+
  labs(title = "Distribution of Percent with Less than HS Education", subtitle = "By Quantile of Percent Misconduct", x = "Percent with Less than High School Education")+
  scale_x_continuous(limits = c(0,65))


data %>% 
  ggplot(aes(x = pct_bachelors))+
  facet_wrap(~nbrokers_quantile)+
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  ft_theme()+
  labs(title = "Distribution of Percent with Less than HS Education", subtitle = "By Quantile of Number of Brokers", x = "Percent with Less than High School Education")+
  scale_x_continuous(limits = c(0,65))
