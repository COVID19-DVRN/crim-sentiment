library(tidyverse)
library(reticulate)

summary_ratings <- read_rds("data/summary_ratings.rds")
source_python("07_sentiment_other.py")

compare_sentiment_poss <- possibly(compare_sentiment_other, otherwise = tibble(.rows = 0))

# call python function
other_ratings <- map_dfr(summary_ratings$sentence, compare_sentiment_poss)

summary_ratings <- summary_ratings %>%
  left_join(other_ratings, by = "sentence") %>%
  # standardize for comparison
  mutate(across(liu_hu:afinn, ~(.x - mean(.x))/sd(.x)))

write_rds(summary_ratings, "data/summary_ratings_all.rds")
summary_ratings <- read_rds("data/summary_ratings_all.rds")

systems <- c("socialsent", "nlp", "vader", "liu_hu", "swn", "afinn")

source_python("08_sentiment_model.py")

res <- fit_models(summary_ratings, systems)

names(res) <- c("errors_predict", "errors_systems", "every_error_regress", "every_error_rf", "all_errors_systems")

write_rds(res, "data/model_results.rds")

mean_difs_predict <- colMeans(res$errors_predict) %>% 
  set_names(c("regression", "random forest"))
mean_difs_algos <- colMeans(res$errors_systems) %>% 
  set_names(systems)

round(mean_difs_algos, 2)
round(mean_difs_predict, 2)

