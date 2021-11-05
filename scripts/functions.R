# Define some useful functions

## Necessary libraries
library(tidyverse)
library(AER)
library(haven)
library(ggthemes)
library(ftplottools)
library(LaplacesDemon)
library(lfe)

## Negate the %in% operator (for convenience)

`%notin%` <- Negate(`%in%`)
