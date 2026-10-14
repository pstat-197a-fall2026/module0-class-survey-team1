# Question 3: What explains spikes in when people submitted the survey? 
# Feature engineering a date variable (maybe)

# assigned at the first lecture, Thu Sep 24 
# Key dates: Tue Sep 29 lecture, Wed Sep 30 section, Thu Oct 1 lecture,
# rather than piling up near the deadline (Sun Oct 4, 11:59 PM)

# The class calendar below is NOT in the dataset; it comes from the course
# materials page and our own record of when announcements were made.
# Data: data/response-timing.csv gives daily counts only (no individual
# timestamps)

library(tidyverse)
library(broom)

response_date <- read_csv("data/response-timing.csv")
View(response_date)

# add key dates

assigned_date <- as.Date('2026-09-24')   # first class
reminder_dates <- as.Date(c('2026-09-29', '2026-09-30', '2026-10-01')) # lec Tues/Thurs and section Wed following week
deadline_date <- as.Date('2026-10-04')
other_assignment <- as.Date('2026-09-28') # reading response and github link assignment

timing <- response_date %>%
  mutate(
    weekday = wday(date, label = TRUE), 
    day_type = case_when(
      date < assigned_date ~ 'Before first class',
      date == assigned_date ~ 'Assigned in class',
      date %in% reminder_dates ~ 'Reminder in class/section',
      date > deadline_date ~ 'After deadline',
      TRUE ~ 'No class contact'
    ),
    weekend = weekday %in% c('Sat','Sun'),
    cum_n = cumsum(n),
    cum_pct = 100*cum_n/sum(n)
  )

timing %>% select(date, weekday, day_type, n, cum_pct) %>% print(n=Inf)

# summary by type of day
# on through assignment deadline (exclude the 1 person early and 1 person late)

window <- timing %>% filter(date >= assigned_date, date <= deadline_date)

day_summary <- window %>%
  group_by(day_type) %>%
  summarise(days = n(),
            responses = sum(n),
            per_day = responses/days,
            .groups = 'drop') %>%
  mutate(share_of_responses = 100 * responses / sum(responses),
         share_of_days = 100 * days /sum(days))
day_summary

# 4 class-contact days of 11 vs. what share of responses?
window %>%
  summarise(contact_share = 100 * sum(n[day_type != 'No class contact']) / sum(n),
            contact_days_share = 100 * mean(day_type != 'No class contact'))

# each reminder fizzles out responses
window %>% filter(day_type == 'Reminder in class/section') %>% select(date, weekday, n)

# Did the other Monday deadline (sep 28) pull survey responses? (n=4, no spike)
timing %>% filter(date == other_assignment)

## PLOT

fill_cols <- c('Assigned in class' = '#2a78d6',
               'Reminder in class/section' = 'gold',
               'No class contact' = 'grey70',
               'Before first class' = 'grey85',
               'After deadline' = 'grey85')

p_timing <- ggplot(timing, aes(x = date, y = n)) +
  geom_col(aes(fill = day_type), width = 0.8) +
  geom_vline(xintercept = assigned_date - 0.5, linetype = 'dashed', colour = 'grey40') + 
  annotate('text', x = assigned_date -2.2, y=14, label = 'Assigned Date\nThur Sep 24',
           hjust = 0, size=3.2, colour = 'grey30') + 
  geom_vline(xintercept = deadline_date + 0.5, linetype = 'dashed', colour = 'grey40') +
  annotate('text', x = deadline_date + 0.4, y = 14, label = 'Deadline\nSun Oct 4',
           hjust = 1, size = 3.2, colour = 'grey30') +
  geom_text(aes(label = n), vjust = -0.4, size = 3.2, colour = 'grey20') +
  scale_fill_manual(values = fill_cols, breaks = names(fill_cols)[1:3], name = NULL) +
  scale_x_date(date_breaks = '1 day', date_labels = '%a\n%b %d', expand = expansion(add = 0.6)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = 'Survey responses submitted',
       title = 'Responses spiked on the days the survey was announced in class') +
  theme_minimal(base_size = 11) +
  theme(legend.position = 'top', panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
p_timing

p_cum <- ggplot(timing, aes(x = date, y = cum_pct)) +
  geom_line(linewidth = 0.8, colour = '#2a78d6') +
  geom_point(size = 2, colour = '#2a78d6') +
  geom_vline(xintercept = c(assigned_date, reminder_dates), colour = 'grey70', linetype = 'dotted') +
  scale_x_date(date_breaks = '2 days', date_labels = '%b %d') +
  labs(x = NULL, y = 'Cumulative % of Responses', title = "Accumulation of All Responses Over Time") +
  theme_minimal(base_size = 11)
p_cum

# Exploratory Poisson Model

# Daily count ~ type of day, within the response window (11 days).
# Very small sample: treat estimates as descriptive, not as tests.
window <- window %>%
  mutate(day_type = fct_relevel(day_type, 'No class contact'))

fit <- glm(n ~ day_type, family = poisson, data = window)
tidy(fit, exponentiate = TRUE, conf.int = TRUE)

# overdispersion check: ratio well above 1 means the Poisson SEs are too small
sum(residuals(fit, type = 'pearson')^2) / df.residual(fit)

# Alternative: a separate effect for the first reminder (Tue) vs. later ones
window <- window %>%
  mutate(contact = case_when(
    date == assigned_date ~ 'Assigned',
    date == reminder_dates[1] ~ 'First reminder (Tue)',
    date %in% reminder_dates[-1] ~ 'Later reminders (Wed/Thu)',
    TRUE ~ 'None'
  ) %>% fct_relevel('None'))

fit2 <- glm(n ~ contact, family = poisson, data = window)
sum(residuals(fit2, type = 'pearson')^2) / df.residual(fit2)  # still > 1

# quasi-Poisson widens the intervals to account for the extra variation;
# this is the version we report
fit2_q <- glm(n ~ contact, family = quasipoisson, data = window)
tidy(fit2_q, exponentiate = TRUE, conf.int = TRUE)



