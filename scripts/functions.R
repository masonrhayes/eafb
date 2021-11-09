# Define some useful functions

## Necessary libraries
library(tidyverse)
library(AER)
library(haven)
library(ggthemes)
library(ftplottools)
library(LaplacesDemon)
library(lfe)
library(stargazer)

## Negate the %in% operator (for convenience)

`%notin%` <- Negate(`%in%`)

## Graph misconduct by state


misconduct_by_state = function(df){
  ggplot(data = df,aes(x = pct_with_misc_records))+
    facet_wrap(~state)+
    geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
    ft_theme()+
    labs(title = "Percent Misconduct by State", x = "Percent Misconduct")+
    scale_x_continuous(limits = c(0,0.3))
}

education_by_state = function(df){
  ggplot(data = df,aes(x = pct_less_than_hs))+
    facet_wrap(~state)+
    geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
    ft_theme()+
    labs(title = "Educational Distribution by State", subtitle = "Distribution of Percent with Less than HS Education", x = "Percent with Less than High School Education")
}
