# Question 2: is there any correlation between preferring an industry project 
# with if you’ve done research or not? What about other variables?

library(tidyverse)
library(broom)

background <- read_csv('../data/background-clean.csv', show_col_types = FALSE)
head(background)