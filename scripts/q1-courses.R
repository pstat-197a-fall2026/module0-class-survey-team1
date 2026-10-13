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
background %>%
  select(upper_div_courses, prog.prof, math.prof, stat.prof) %>%
  pivot_longer(-upper_div_courses, names_to = 'skill', values_to = 'level') %>%
  mutate(skill = recode(skill, prog.prof = 'Programming',
                        math.prof = 'Math', stat.prof = 'Statistics'),
         skill = factor(skill, levels = c('Programming', 'Math', 'Statistics'))) %>%
  ggplot(aes(x = upper_div_courses, fill = level)) +
  geom_bar(position = position_fill(reverse = TRUE)) +   # beg at bottom
  facet_wrap(~ skill) +
  scale_x_discrete(labels = c('3-5' = '3-5\n(n=2)',       # show group sizes
                              '6-8' = '6-8\n(n=19)',
                              '9+'  = '9+\n(n=30)')) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(beg = '#E69F00',
                               int = '#999999',
                               adv = '#0072B2'),
                    labels = c(beg = 'Beginner',
                               int = 'Intermediate',
                               adv = 'Advanced')) +
  labs(x = 'Upper-division courses taken', y = 'Proportion of students',
       fill = 'Proficiency',
       title = 'Self-rated proficiency by number of upper-division courses') +
  theme_minimal()



## EDA: distribution of comfort scores
background %>%
  select(prog.comf, math.comf, stat.comf) %>%
  pivot_longer(everything(), names_to = 'skill', values_to = 'comfort') %>%
  mutate(skill = recode(skill, prog.comf = 'Programming',
                        math.comf = 'Math', stat.comf = 'Statistics'),
         skill = factor(skill, levels = c('Programming', 'Math', 'Statistics'))) %>%
  ggplot(aes(x = comfort)) +
  geom_histogram(binwidth = 1, fill = '#0072B2', color = 'white') +
  facet_wrap(~ skill) +
  scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +
  labs(x = 'Comfort score (1 = low, 5 = high)', y = 'Number of students',
       title = 'Distribution of self-rated comfort') +
  theme_minimal()

## EDA: course data
# one row per student-course pair
course_long <- background %>%
  select(response_id, courses) %>%
  separate_rows(courses, sep = ',\\s*') %>%
  mutate(courses = str_trim(courses)) %>%
  filter(courses != '') %>%
  distinct(response_id, courses)

# how many students took each course
course_counts <- course_long %>%
  count(courses, name = 'n') %>%
  arrange(desc(n))

course_counts


## how many students took each course
course_counts %>%
  ggplot(aes(x = n, y = fct_reorder(courses, n))) +
  geom_col(fill = '#0072B2') +
  geom_text(aes(label = n), hjust = -0.3, size = 3) +
  geom_vline(xintercept = nrow(background), linetype = 'dashed') +   # all 51
  labs(x = 'Number of students who took it', y = NULL,
       title = 'Courses taken by students (dashed line = all 51)') +
  theme_minimal()


## upper-division course counts
background %>%
  ggplot(aes(x = upper_div_courses)) +
  geom_bar(fill = '#0072B2', width = 0.6) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(x = 'Upper-division courses taken', y = 'Number of students',
       title = 'Upper-division course counts') +
  theme_minimal()


## heat map of which courses are taken together
# wide table: one row per student, one 0/1 column per course
course_wide <- course_long %>%
  mutate(taken = 1) %>%
  pivot_wider(names_from = courses, values_from = taken, values_fill = 0)

# correlation between every pair of courses
course_order <- course_counts$courses    # most to least common

course_cor <- course_wide %>%
  select(-response_id) %>%
  cor() %>%
  as_tibble(rownames = 'course1') %>%
  pivot_longer(-course1, names_to = 'course2', values_to = 'r') %>%
  mutate(course1 = factor(course1, levels = course_order),
         course2 = factor(course2, levels = rev(course_order)))

course_cor %>%
  ggplot(aes(x = course1, y = course2, fill = r)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = round(r, 2)), size = 2.2) +
  scale_fill_gradient2(low = '#2166AC', mid = 'white', high = '#B2182B',
                       midpoint = 0, limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = 'Correlation',
       title = 'Which courses tend to be taken together') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))