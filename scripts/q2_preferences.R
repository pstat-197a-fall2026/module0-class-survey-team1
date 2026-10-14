# Question 2: is there any relationship between preferring an industry project 
# with if you’ve done research or not? What about other variables?

library(tidyverse)
library(broom)
library(kableExtra)

# The data for if they've done research before is in the `background-clean`
# data set while the preference for industry or research is in the
# interest-clean data set

background <- read_csv('../data/background-clean.csv', show_col_types = FALSE)
interest <- read_csv('../data/interest-clean.csv', show_col_types = FALSE)
head(background) |> kable()
head(interest) |> kable()

# To observe the relationship between the two variables we need to merge
# the dataframes. Note that we will only have 49 observations since one of the
# datasets only has 49 responses, not 51

joint <- background |> 
  inner_join(interest, by = 'response_id')

nrow(joint)

# EXPLORATORY DATA ANALYSIS

# UNIVARIATE ANALYSIS

# Count observations in each group

# The majority of responses preferred industry projects.
# There's also a sizeable amount of people who had no preference
# But only a few people preferred research over industry
joint |> group_by(type) |> 
  count()

# There is a nearly even split of people who have and haven't conducted research
joint |> group_by(research) |> 
  count()

# univariate bar plots
ggplot(joint, aes(x = type)) +
  geom_bar(fill = "steelblue")

ggplot(joint, aes(x = research)) +
  geom_bar(fill = "coral")

# BIVARIATE ANALYSIS

# Contingency table of the two variables
table(joint$research, joint$type)

ggplot(joint, aes(x = type, fill = research)) +
  geom_bar() +
  labs(
    x = "Preferred Project",
    y = "COunt",
    fill = "Did Research?"
  )

# Based on this proportion stacked bar chart, there does not seem to be
# a signficiant difference in project preference
ggplot(joint, aes(x = type, fill = research)) +
  geom_bar(position = "fill") +
  labs(
    x = "Preferred Project",
    y = "Frequency",
    fill = "Did Research?"
  )

# However, a bar chart itself is not enough to conclude significance.
# We will use a significance test to determine if there's any tangible
# relationship between the two variables.
