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

# analysis:
# there looks to be some correlation with proficiency and number of upper div 
# classes taken before the capstone series
# statistics had more advance scores, this makes sense since most of the class
# body are statistics majors
# Programming had the only beginner responses among the lower end upper div classes taken
# this makes sense as there are not many programming-centric classes in the p tat
# department, most of R or python could have been learned throughout the major




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
# no student ever reported a score of a 1 or rarely a two, it seems that programming 
# might be a weaker overall skill compared to math or stats, maybe students take more theoretical 
# course loads that may have skewed these results


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

# 1 PSTAT 126    50
# 2 PSTAT 120    49
# 3 PSTAT 131    46
# 4 PSTAT 122    43
# 5 CS 9         40
# it might be worth dropping these courses because comparing a near absolute majority
# to a small minority of students who have yet to take these courses
# its also worth mentioning that with the exception of 131, all these other courses are
# pre-requisite major courses, either there is data entry error or students received credit
# for cs9 previously at a different institution
# 131 is an elective intriguing that its the most popular among all electives, why is that?
# current ml AI trend landscape could be responsible

# one might think to drop the cs 130 variable, but it is an upper div cs class
# could this student have chosen higher comfortability or proficiency scores?


## upper-division course counts
background %>%
  ggplot(aes(x = upper_div_courses)) +
  geom_bar(fill = '#0072B2', width = 0.6) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(x = 'Upper-division courses taken', y = 'Number of students',
       title = 'Upper-division course counts') +
  theme_minimal()
# more than have the student population have taken at lest 9 upperdiv courses
# and almost all students have taken at least 6
# only 2 students have taken at most 5 upperdiv courses, this may have inadvertently 
# exposed juniors in the course, or anyone on their last year who is finishing up all
# their final elective units along with the capstone series



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

# courses like cs16 and cs9 make sense to have moderate negative correlation as 
# our major sheet allows one or the other to be taken (with cs9 preference)

# classes with high or low correlations could be a tested to by the fact that
# only certain class offering per quarter are available and may tend to appear together
# so this may mot show any indication that a student has taken this with that,
# but rather one was available while they were interested in taking another course


# in addition many of these classes did not have many students taking them, 
# this heat map inst an indication of propper correlation of classes taken together but 
# may give insight on what popular classes were probably taken together by students




#Building Score-------

# building the ability score
## BUILDING THE SCORES: options considered
# overall averages (one score per student):
#   prof_avg = avg of 3 proficiency scores (1-3 scale, only 5 possible values)
#   comf_avg = avg of 3 comfort scores (1-5 scale, 8 possible values)
#   + one simple model answers "which course has the biggest difference"
#   - blends skills together; EDA showed programming and stats behave differently
#
# per-skill scores (three scores per student):
#   prog/math/stat proficiency (.prof.num) or comfort (.comf)
#   + shows which course goes with which skill (e.g. CS 9 -> programming)
#   - 3 models to run; math and stat proficiency only have 2 values (int/adv)
#
# proficiency vs comfort:
#   proficiency is closer to "perceived ability" but has little spread
#   comfort has more spread, better suited to regression
# 
#
# caveats:
#   beg/int/adv = 1/2/3 assumes equal spacing
#   most students rate themselves high
#   self reported reflects confidence, not tested ability
#
# plan:
#   main  -> prof_avg or comf_avg (pick one, justify)
#   check -> same model on the other average, see if results agree
#   extra -> per-skill comfort models to see which courses go with which skill

## build score: overall proficiency average
# prof_avg as the main outcome
# (closest to perceived ability (comfort and per-skill options group members ? :O)
background <- background %>%
  mutate(prof_avg = (prog.prof.num + math.prof.num + stat.prof.num) / 3)

# check the new score
summary(background$prof_avg)
background %>% count(prof_avg)

# quick look at the distribution
background %>%
  ggplot(aes(x = prof_avg)) +
  geom_bar(fill = '#0072B2') +
  labs(x = 'Average proficiency (1 = beginner, 3 = advanced)',
       y = 'Number of students',
       title = 'Overall self-rated proficiency') +
  theme_minimal()


# prof_avg = (programming + math + stats) / 3, with beg = 1, int = 2, adv = 3
# example: int, int, adv -> (2 + 2 + 3) / 3 = 2.33
# each student's proficiency answers converted to numbers: beg = 1, int = 2, adv = 3

#   drop PSTAT 120, 122, 126: prerequisites, nearly everyone took them
#   drop courses taken by 3 or fewer students (LING 110, LING 104, GEOG 186, CS 130)
#   keep everything else for now
course_wide_clean <- course_wide %>%
  rename_with(~ str_remove_all(.x, ' '), -response_id)

course_split <- course_counts %>%
  mutate(taken = n,
         not_taken = nrow(background) - n) %>%
  select(courses, taken, not_taken)

keep_courses <- course_split %>%
  filter(!courses %in% c('PSTAT 120', 'PSTAT 122', 'PSTAT 126'),
         taken > 3) %>%
  pull(courses) %>%
  str_remove_all(' ')

keep_courses   # expect 11 courses

# regression data: one row per student, score + kept courses
q1_data <- background %>%
  select(response_id, prof_avg, upper_div_courses) %>%
  left_join(course_wide_clean %>% select(response_id, all_of(keep_courses)),
            by = 'response_id')

nrow(q1_data)   # expect 51
glimpse(q1_data)

q1_model <- lm(prof_avg ~ PSTAT131 + CS9 + PSTAT160 + PSTAT100 + PSTAT134 +
                 PSTAT174 + CS16 + PSTAT115 + PSTAT127 + PSTAT175 + CS5,
               data = q1_data)

summary(q1_model)

# coefficients with 95% confidence intervals, largest first
q1_coefs <- tidy(q1_model, conf.int = TRUE) %>%
  filter(term != '(Intercept)') %>%
  arrange(desc(estimate))

q1_coefs

# coefficient plot
q1_coefs %>%
  ggplot(aes(x = estimate, y = fct_reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray50') +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, orientation = 'y') +
  geom_point(size = 2.5, color = '#0072B2') +
  labs(x = 'Difference in average proficiency (took vs. did not take)',
       y = NULL,
       title = 'Course-taking and self-rated proficiency',
       subtitle = 'Points = estimates, bars = 95% confidence intervals') +
  theme_minimal()



# immediate analysis

# results:
# no course showed a statistically significant difference in prof_avg (all p > 0.05)
# PSTAT 131 had the largest difference (+0.37, p = 0.06), but only 5 of 51
#   students had not taken it, so the estimate compares 46 students to 5
#   and is not reliable
# the overall model was not significant (p = 0.47, adjusted R^2 ~ 0)
# two possible explanations:
#   1. small sample (n = 51, 11 predictors) -> too little power to detect effects
#   2. courses taken may have little connection to self-rated proficiency
# we can't tell these apart with this data; results should be read as exploratory

## robustness check: refit with only the larger courses
# dropped PSTAT 131 (only 5 non-takers) and PSTAT 115, 127, 175, CS 5 (5-7 takers)
# kept courses with at least 11 students in both the taken and not-taken groups
q1_model_small <- lm(prof_avg ~ CS9 + PSTAT160 + PSTAT100 + PSTAT134 +
                       PSTAT174 + CS16,
                     data = q1_data)

summary(q1_model_small)

q1_coefs_small <- tidy(q1_model_small, conf.int = TRUE) %>%
  filter(term != '(Intercept)') %>%
  arrange(desc(estimate))

q1_coefs_small

# compare both models in one plot
bind_rows(
  q1_coefs %>% mutate(model = 'All 11 courses'),
  q1_coefs_small %>% mutate(model = 'Larger 6 courses')
) %>%
  ggplot(aes(x = estimate, y = fct_reorder(term, estimate), color = model)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray50') +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.2, orientation = 'y',
                position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c('All 11 courses' = '#999999',
                                'Larger 6 courses' = '#0072B2')) +
  labs(x = 'Difference in average proficiency (took vs. did not take)',
       y = NULL, color = NULL,
       title = 'Course-taking and self-rated proficiency') +
  theme_minimal() +
  theme(legend.position = 'bottom')


# not much difference in conclusion


## model with ALL courses 
q1_data_all <- background %>%
  select(response_id, prof_avg) %>%
  left_join(course_wide_clean, by = 'response_id')

# '.' = every column except the ones removed with '-'
q1_model_all <- lm(prof_avg ~ . - response_id, data = q1_data_all)

summary(q1_model_all)

q1_coefs_all <- tidy(q1_model_all, conf.int = TRUE) %>%
  filter(term != '(Intercept)') %>%
  arrange(desc(estimate))

q1_coefs_all

q1_coefs_all %>%
  ggplot(aes(x = estimate, y = fct_reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray50') +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.2, orientation = 'y') +
  geom_point(size = 2.5, color = '#0072B2') +
  labs(x = 'Difference in average proficiency (took vs. did not take)',
       y = NULL, title = 'All 18 courses') +
  theme_minimal()

# no difference in conclusion nothing is statistically significant