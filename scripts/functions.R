# Define some useful functions

## Necessary libraries
library(tidyverse)
library(AER)
library(haven)

## Negate the %in% operator (for convenience)

`%notin%` <- Negate(`%in%`)
