library(reticulate)
library(tidyverse)

source_python("04_sentiment_functions.py")

ratings <- map_dfr(list.files("data", "ratings.\\.csv", full.names = TRUE),
                   read_csv,
                   .id = "rater")

summary_ratings <- ratings %>%
  group_by(sentence) %>%
  summarise(
    mean_us = mean(score, na.rm = TRUE),
    median_us = median(score, na.rm = TRUE),
    min_us = min(score, na.rm = TRUE),
    max_us = max(score, na.rm = TRUE),
    sd_us = sd(score, na.rm = TRUE)
  )

compare_sentiment_poss <- possibly(compare_sentiment, otherwise = tibble(.rows = 0))

# call python function
other_ratings <- map_dfr(summary_ratings$sentence,
                     compare_sentiment_poss,
                     socialsent_reg,
                     stop_words)

summary_ratings <- other_ratings %>%
  left_join(summary_ratings, by = "sentence") %>%
  mutate(nlp = parse_number(nlp))

summary_ratings <- summary_ratings %>%
  filter(!is.na(socialsent)) %>%
  mutate(us = mean_us) %>%
  relocate(us, .before = "mean_us") %>%
  # standardize for comparison
  mutate(across(socialsent:us, ~(.x - mean(.x))/sd(.x)))

write_csv(summary_ratings, "data/summary_ratings.csv")

# list of each of the words in the sentence
summary_ratings <- summary_ratings %>%
  mutate(words = {
    sentence %>%
      str_to_lower() %>%
      textstem::lemmatize_strings() %>%
      str_replace_all("covid", "coronavirus") %>%
      str_replace_all("correctional", "correction") %>%
      str_replace_all("die", "death") %>%
      map(word_tokenize)
  })

write_rds(summary_ratings, "data/summary_ratings.rds")


