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
# Programming had the most beginner responses among the lower end upper div classes taken
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
# more than have the student population have taken  more than 9 upperdiv coursesw
# and almost all students have taken more than 6
# only 2 students have taken at most 5 upperdiv courses, this may have inadverdantly 
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

# courses like cs16 and cs9 make sense to have a very high negative correlation as 
# our major sheet allows one or the other to be taken (with cs9 preference)

# classes with high or low correlations could be a tested to by the fact that
# only certain class offering per quarter are available and may tend to appear together
# so this may mot show any indication that a student has taken this with that,
# but rather one was available while they were interested in taking another course


# in addition many of these classes did not have many students taking them, 
# this heat map inst an indication of propper correlation of classes taken together but 
# may give insight on what popular classes were probably taken together by students
# 