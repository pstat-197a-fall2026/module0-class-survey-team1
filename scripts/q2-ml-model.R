# Q2: can background variables predict an industry preference?
# Industry = 32; lab or no preference = 17.
# Models: ridge logistic regression (lambda = 0.05) and a depth-2 classification tree.

library(tidyverse)
library(glmnet)
library(rpart)

# 1. Data ----
background <- read_csv('data/background-clean.csv', show_col_types = FALSE)
interest <- read_csv('data/interest-clean.csv', show_col_types = FALSE)
# Merge 49 respondents; proficiency is 1/2/3 and Other combines lab/no preference.
d <- background %>%
  inner_join(select(interest, response_id, type), by = 'response_id') %>%
  mutate(across(ends_with('.prof'), ~ match(.x, c('beg', 'int', 'adv'))),
         preference = factor(if_else(type == 'ind', 'Industry', 'Other'),
                             levels = c('Other', 'Industry')),
         research = factor(research, levels = c('No', 'Yes', 'Unsure')),
         domain_specialization = factor(domain_specialization,
                                        levels = c('No', 'Yes', 'Unsure')),
         upper_div_group = factor(if_else(upper_div_courses == '9+', '9+', '8 or fewer'),
                                  levels = c('8 or fewer', '9+')))
formula <- preference ~ prog.prof + math.prof + stat.prof +
  prog.comf + math.comf + stat.comf + research + domain_specialization + upper_div_group

# Stratify test folds by outcome.
folds <- function(y, k) {
  result <- integer(length(y))
  for (class in unique(y)) {
    pos <- which(y == class)
    result[pos] <- sample(rep(seq_len(k), length.out = length(pos)))
  }
  result
}

score <- function(y, p) {
  prediction <- p >= 0.5
  sensitivity <- mean(prediction[y == 1])
  specificity <- mean(!prediction[y == 0])
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  auc <- (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  tibble(accuracy = mean(prediction == y),
         balanced_accuracy = (sensitivity + specificity) / 2,
         industry_recall = sensitivity, other_recall = specificity,
         auc)
}

# 2. Fit both models with the same predictors and balanced class weights ----
lambda <- 0.05
fit_models <- function(data) {
  train_x <- model.matrix(formula, data)[, -1, drop = FALSE]
  train_y <- as.integer(data$preference == 'Industry')
  # Give both groups equal total training weight.
  data$weight <- ifelse(train_y == 1, nrow(data) / (2 * sum(train_y == 1)),
                                     nrow(data) / (2 * sum(train_y == 0)))
  list(
    logistic = glmnet(train_x, train_y, family = 'binomial', alpha = 0,
                       lambda = lambda, standardize = TRUE, weights = data$weight),
    tree = rpart(formula, data = data, weights = weight, method = 'class',
                  control = rpart.control(maxdepth = 2, minbucket = 8,
                                          minsplit = 16, cp = 0, xval = 0))
  )
}

# 3. Repeated five-fold validation, shared by both models ----
# Average held-out scores over 10 five-fold repeats of the same 49 students.
evaluate_models <- function(data) {
  actual <- as.integer(data$preference == 'Industry')
  test_x <- model.matrix(formula, data)[, -1, drop = FALSE]
  set.seed(2026101401)
  predictions <- list()
  run <- 0L
  for (repeat_id in seq_len(10)) {
    fold_id <- folds(actual, 5)
    for (fold in seq_len(5)) {
      run <- run + 1L
      train <- fold_id != fold
      models <- fit_models(data[train, ])
      predictions[[run]] <- tibble(
        repeat_id, fold, response_id = data$response_id[!train], actual = actual[!train],
        logistic_p = as.numeric(predict(models$logistic, test_x[!train, , drop = FALSE],
                                        s = lambda, type = 'response')),
        tree_p = predict(models$tree, data[!train, ], type = 'prob')[, 'Industry']
      )
    }
  }
  predictions <- bind_rows(predictions)
  metrics <- predictions %>% group_by(repeat_id) %>%
    group_modify(~ bind_rows(
      score(.x$actual, .x$logistic_p) %>% mutate(model = 'Logistic'),
      score(.x$actual, .x$tree_p) %>% mutate(model = 'Tree'),
      score(.x$actual, rep(1, nrow(.x))) %>% mutate(model = 'Baseline')
    )) %>% ungroup()
  results <- metrics %>%
    group_by(model) %>%
    summarise(across(c(accuracy, balanced_accuracy, auc, industry_recall, other_recall),
                     mean), .groups = 'drop') %>%
    mutate(model = factor(model, levels = c('Baseline', 'Logistic', 'Tree'))) %>%
    arrange(model)
  list(predictions = predictions, metrics = metrics, results = results)
}
validation <- evaluate_models(d)
ml_results <- validation$results
print(ml_results %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)

# 4. Prediction performance ----
ml_plot <- ml_results %>%
  select(model, accuracy, balanced_accuracy, auc) %>%
  pivot_longer(-model, names_to = 'metric', values_to = 'score') %>%
  mutate(metric = factor(metric, levels = c('accuracy', 'balanced_accuracy', 'auc'),
                          labels = c('Accuracy', 'Balanced accuracy', 'AUC'))) %>%
  ggplot(aes(model, score)) +
  geom_col(fill = 'steelblue', width = 0.65) +
  geom_text(aes(label = sprintf('%.3f', score)), vjust = -0.5, size = 3.5) +
  facet_wrap(~ metric, nrow = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = 'Prediction performance', x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
print(ml_plot)

# 5. Model diagrams fitted to all 49 respondents ----
# Use full-data fits for the model diagrams.
models <- fit_models(d)
coefficients <- as.matrix(coef(models$logistic, s = lambda))
coefficient_data <- tibble(variable = rownames(coefficients), coefficient = coefficients[, 1]) %>%
  filter(variable != '(Intercept)') %>%
  mutate(variable = recode(variable,
    researchYes = 'research: Yes vs No', researchUnsure = 'research: Unsure vs No',
    domain_specializationYes = 'specialization: Yes vs No',
    domain_specializationUnsure = 'specialization: Unsure vs No',
    `upper_div_group9+` = 'courses: 9+ vs 8 or fewer'))

logistic_plot <- ggplot(coefficient_data, aes(coefficient, reorder(variable, coefficient))) +
  geom_col(fill = 'steelblue', width = 0.65) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(title = 'Ridge logistic regression', x = 'Coefficient (log-odds of Industry)', y = NULL) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
print(logistic_plot)

# Show the variable at each node and the actual values on each branch.
tree_positions <- plot(models$tree, uniform = TRUE, margin = 0.12)
frame <- models$tree$frame
node <- as.integer(rownames(frame))
child <- which(node != 1)
parent <- match(node[child] %/% 2, node)
node_labels <- recode(frame$var, stat.prof = 'Statistics proficiency',
                      research = 'Research experience',
                      domain_specialization = 'Domain specialization')
leaves <- frame$var == '<leaf>'
node_labels[leaves] <- levels(d$preference)[frame$yval[leaves]]
node_labels <- paste0(node_labels, '\nn=', frame$n)

x <- tree_positions$x
y <- tree_positions$y
half_width <- strwidth(node_labels, cex = 0.9) / 2 + 0.04
half_height <- strheight(node_labels, cex = 0.9) / 2 + 0.015
rect(x - half_width, y - half_height, x + half_width, y + half_height,
     col = 'white', border = NA)
text(x, y, node_labels, cex = 0.9)

branch_labels <- sub('^[^<>=]+=?', '', labels(models$tree, pretty = 0)[child])
branch_labels <- str_replace_all(branch_labels, ',', ' / ')
branch_labels[frame$var[parent] == 'stat.prof'] <- recode(
  gsub(' ', '', branch_labels[frame$var[parent] == 'stat.prof']),
  '>=2.5' = 'Advanced', '<2.5' = 'Beginner / Intermediate')
text(x[child], (y[child] + y[parent]) / 2, branch_labels,
     pos = ifelse(node[child] %% 2 == 0, 2, 4), cex = 0.9)
title('Decision tree')

# 6. One-sided permutation tests of model balanced accuracy ----
# Shuffle labels and rerun validation; adjust the two model p-values with BH.
n_permutations <- 499
set.seed(20261014)
shuffled_labels <- replicate(n_permutations, sample(d$preference), simplify = FALSE)
null_scores <- matrix(NA_real_, n_permutations, 2,
                      dimnames = list(NULL, c('Logistic', 'Tree')))
for (i in seq_len(n_permutations)) {
  permuted <- d
  permuted$preference <- shuffled_labels[[i]]
  result <- evaluate_models(permuted)$results
  null_scores[i, ] <- result$balanced_accuracy[match(colnames(null_scores), result$model)]
  if (i %% 50 == 0) {
    cat('Permutations:', i, '/', n_permutations, '\n')
    flush.console()
  }
}
significance <- ml_results %>%
  filter(model != 'Baseline') %>%
  select(model, balanced_accuracy) %>%
  mutate(p_value = c(
    (1 + sum(null_scores[, 'Logistic'] >= balanced_accuracy[1])) / (n_permutations + 1),
    (1 + sum(null_scores[, 'Tree'] >= balanced_accuracy[2])) / (n_permutations + 1)),
    p_adjusted = p.adjust(p_value, method = 'BH'))
print(significance, width = Inf)

# FINDINGS
# Test accuracy was 36.3% for ridge logistic regression and 42.2% for the tree,
# compared with 65.3% for always predicting Industry. Balanced accuracy was
# 33.9% and 41.3%; AUC was 0.335 and 0.358, indicating weak discrimination.
# Research: unsure vs no had the largest positive ridge coefficient; stat.prof was negative.
# The tree split on statistics proficiency, followed by research and specialization.
# Permutation p-values were 0.972 and 0.872; both BH-adjusted p-values were 0.972.
# Neither model significantly outperformed shuffled labels on balanced accuracy.
# Other parameter settings did not yield a consistent improvement over the baseline.
