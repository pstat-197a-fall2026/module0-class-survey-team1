# Question 1: Which courses are associated with differences in perceived ability?
# Sub-question: does the number of upper-division courses taken matter?

library(tidyverse)
library(broom)

# 51 students who consented
background <- read_csv('data/background-clean.csv', show_col_types = FALSE)

nrow(background)              # expect 51
glimpse(background)
colSums(is.na(background))    # any missing values?

# set low -> high
proficiency_levels <- c('beg', 'int', 'adv')

# make proficiency ordered factors (beg < int < adv)
background <- background %>%
  mutate(across(ends_with('.prof'),
                ~ factor(.x, levels = proficiency_levels, ordered = TRUE)))

# numeric = 1, int = 2, adv = 3
background <- background %>%
  mutate(across(ends_with('.prof'), as.numeric, .names = '{.col}.num'))

# set up category ranges (3-5 < 6-8 < 9+)
background <- background %>%
  mutate(upper_div_courses = factor(upper_div_courses,
                                    levels = c('3-5', '6-8', '9+'),
                                    ordered = TRUE))

# check mutations
background %>% count(prog.prof, prog.prof.num)   # beg-1, int-2, adv-3
background %>% count(upper_div_courses)

# mini eda 
## EDA: proficiency by number of upper-division courses
# merge 3-5 into 6-8 (only 2 students in 3-5)
background <- background %>%
  mutate(upper_div_group = fct_collapse(upper_div_courses,
                                        '8 or fewer' = c('3-5', '6-8'),
                                        '9+' = '9+'))

background %>%
  select(upper_div_group, prog.prof, math.prof, stat.prof) %>%
  pivot_longer(-upper_div_group, names_to = 'skill', values_to = 'level') %>%
  mutate(skill = recode(skill, prog.prof = 'Programming',
                        math.prof = 'Math', stat.prof = 'Statistics'),
         skill = factor(skill, levels = c('Programming', 'Math', 'Statistics'))) %>%
  ggplot(aes(x = upper_div_group, fill = level)) +
  geom_bar(position = position_fill(reverse = TRUE)) +   # beg at bottom
  facet_wrap(~ skill) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(beg = '#E69F00',          # orange
                               int = '#999999',          # gray
                               adv = '#0072B2'),         # blue
                    labels = c(beg = 'Beginner',
                               int = 'Intermediate',
                               adv = 'Advanced')) +
  labs(x = 'Upper-division courses taken', y = 'Proportion of students',
       fill = 'Proficiency',
       title = 'Self-rated proficiency by number of upper-division courses') +
  theme_minimal()

