library(tidyverse)
library(patchwork)
library(ggtext)
library(reticulate)


use_virtualenv("proj13")

summary_ratings <- read_rds("data/summary_ratings.rds")
source_python("04_sentiment_functions.py")

# find 100 most-used words
all_words <- pull(summary_ratings, words) %>%
  flatten_chr()

rm_words <- c(
  "january", "february", "march", "april", "may",
  "june", "july", "august", "september", "october",
  "november", "december", "monday", "tuesday", "wednesday",
  "thursday", "friday", "saturday", "sunday",
  "one", "two", "three", "four", "five", "six",
  "seven", "eight", "nine", "ten",
  "could", "also", "across", "would", "since",
  "said", "say", "says", "according", "many", "among", "19",
  "even", "like", "ohio", "us", "within", "accord", "new", "year",
  "get", "ask", "call", "go", "less", "little", "due", "give",
  "day", "week", "still", "back", "make", "much", "come", "take", "see"
)

stop_words_str <- c(
  "few", "most", "he", "can", "you'd", "themselves", "be", "shan't",
  "very", "doing", "ma", "just", "mustn't", "won", "below", "only", "at", "my", "his",
  "again", "while", "mightn't", "you've", "some", "your", "that'll", "she's", "s", "shouldn",
  "been", "does", "needn", "its", "who", "do", "down", "no", "d", "until", "all", "then",
  "yourself", "am", "you're", "re", "doesn", "needn't", "herself", "against", "after", "so",
  "what", "ourselves", "aren't", "if", "you'll", "but", "when", "any", "shouldn't", "won't",
  "should", "as", "above", "couldn't", "you", "hers", "had", "don", "is", "of", "how",
  "where", "those", "this", "wasn't", "to", "there", "by", "own", "being", "yourselves",
  "for", "wouldn't", "y", "they", "each", "such", "doesn't", "myself", "aren", "him",
  "weren't", "ll", "nor", "wouldn", "than", "both", "me", "i", "are", "through", "ours",
  "during", "up", "a", "between", "should've", "wasn", "more", "she", "out", "it", "hadn't",
  "haven", "that", "we", "not", "in", "ain", "before", "them", "here", "isn't", "didn't",
  "over", "weren", "did", "the", "it's", "isn", "didn", "under", "haven't", "off", "don't",
  "hadn", "about", "whom", "from", "her", "an", "same", "having", "have", "now", "himself",
  "were", "hasn't", "t", "yours", "why", "o", "theirs", "and", "or", "on", "has", "mustn",
  "mightn", "too", "because", "will", "m", "their", "further", "ve", "was", "couldn", "our",
  "itself", "which", "into", "once", "shan", "with", "these", "other", "hasn"
)

clean_words <- tibble(words = all_words) %>%
  count(words) %>%
  filter(!words %in% stop_words_str) %>%
  filter(!words %in% rm_words) %>%
  filter(!words %in% c("-", ",", ".", "’", "``", "''", "(", ")", ":", "'re", "'s",
                       "–", ";", "!", "?", "...", "…", "'", "'m", "‘", "/", 
                       "&", "%", "$")) %>%
  count(words, wt = n) %>%
  mutate(number = parse_number(words)) %>%
  filter(is.na(number))



top_words <- clean_words %>%
  filter(n > 10) %>%
  arrange(desc(n))

nrow(top_words)
nrow(top_words) / nrow(clean_words)

# create a logical variable for each of those words
summary_ratings <- summary_ratings %>%
  rename(article_sentence = sentence) %>%
  mutate(any_words = map(words, ~ top_words$words %in% .))

summary_ratings_wide <- transpose(summary_ratings$any_words) %>%
  set_names(top_words$words) %>%
  as_tibble() %>%
  mutate_all(unlist) %>%
  bind_cols(summary_ratings)

# make long dataset with words in rows
by_word <- summary_ratings_wide %>%
  pivot_longer(-any_of(names(summary_ratings)),
    names_to = "word", values_to = "in_sentence"
  ) %>%
  filter(in_sentence)

# average across the words
word_summaries <- by_word %>%
  group_by(word) %>%
  summarise(across(socialsent:us,
    list(mean = mean, sd = sd, n = length),
    .names = "{col}.{fn}"
  )) %>%
  mutate(word = fct_reorder(word, us.mean)) %>%
  pivot_longer(-c(word),
    names_pattern = "(.*)\\.(.*)",
    names_to = c("method", ".value")
  ) %>%
  mutate(
    method = fct_relevel(method, "us"),
    method = fct_rev(method),
    t = qt(.975, df = n - 1),
    se = sd / sqrt(n),
    lci = mean - (t * se),
    uci = mean + (t * se)
  )

word_summaries %>%
  filter(word %in% c("safety", "attorney", "community", "disease", "positive", "virus", "outbreak", "spread", "pandemic")) %>%
  mutate(val = str_glue("{scales::number(mean, .01)} ({scales::number(lci, .01)}, {scales::number(uci, .01)})\n")) %>%
  select(word, method, val) %>%
  pivot_wider(names_from = "method", values_from = "val")

word_summaries %>%
  mutate(val = str_glue("{scales::number(mean, .01)} ({scales::number(lci, .01)}, {scales::number(uci, .01)})\n")) %>%
  select(word, method, val) %>%
  pivot_wider(names_from = "method", values_from = "val") %>%
  write_csv("data/top_words.csv")

crim_words <- c(
  "release", "detainee", "incarcerate", "sheriff", "jail", "federal", "inmate",
  "correction", "prisoner", "prison", "justice", "police", "officer",
  "sentence", "detention", "facility", "center"
)

word_us_ref <- by_word %>%
  group_by(word) %>%
  summarise(across(socialsent:us,
    list(mean = mean, sd = sd, n = length),
    .names = "{col}.{fn}"
  )) %>%
  mutate(
    word = fct_reorder(word, us.mean),
    VADER = vader.mean - us.mean,
    SocialSent = socialsent.mean - us.mean,
    CoreNLP = nlp.mean - us.mean
  ) %>%
  select(word, VADER, SocialSent, CoreNLP) %>%
  mutate(
    discrepant = VADER * SocialSent < 0 | VADER * CoreNLP < 0 | SocialSent * CoreNLP < 0,
    crim = word %in% crim_words,
    hjust_v = ifelse(VADER < 0, "right", "left"),
    hjust_s = ifelse(SocialSent < 0, "right", "left"),
    hjust_c = ifelse(CoreNLP < 0, "right", "left"),
    word_lab = ifelse(!discrepant, str_glue("{word}*"), str_glue("{word}")),
    word_lab = ifelse(crim, str_glue(" **{word_lab}** "), str_glue(" {word_lab} "))
  )


#### FIGURE 2 ####
nlp_comp <- ggplot(word_us_ref) +
  geom_col(aes(CoreNLP, word, fill = CoreNLP)) +
  geom_richtext(aes(CoreNLP, word,
    label = word_lab,
    hjust = hjust_c
  ),
  size = 2.3, color = "dimgrey", vjust = "middle",
  fill = NA, label.color = NA,
  label.padding = grid::unit(rep(2, 4), "pt")
  ) +
  scale_fill_gradient2() +
  coord_cartesian(xlim = c(-1.7, 1.9)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 9.5, color = "black", face = "bold"),
    axis.text.x = element_text(size = 9, color = "dimgrey"),
    axis.ticks.x = element_line(color = "dimgrey"),
    axis.ticks.length.x = unit(1, "mm")
  ) +
  scale_x_continuous(breaks = c(-1, 0, 1)) +
  labs(subtitle = "      iii) CoreNLP (2014)\n")

vader_comp <- ggplot(word_us_ref) +
  geom_col(aes(VADER, word, fill = VADER)) +
  geom_richtext(aes(VADER, word,
    label = word_lab,
    hjust = hjust_v
  ),
  size = 2.3, color = "dimgrey", vjust = "middle",
  fill = NA, label.color = NA,
  label.padding = grid::unit(rep(2, 4), "pt")
  ) +
  scale_fill_gradient2() +
  coord_cartesian(xlim = c(-1.7, 1.9)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 9.5, color = "black", face = "bold"),
    axis.text.x = element_text(size = 9, color = "dimgrey"),
    axis.ticks.x = element_line(color = "dimgrey"),
    axis.ticks.length.x = unit(1, "mm")
  ) +
  scale_x_continuous(breaks = c(-1, 0, 1)) +
  labs(subtitle = "      ii) VADER (2014)\n")

ss_comp <- ggplot(word_us_ref) +
  geom_col(aes(SocialSent, word, fill = SocialSent)) +
  geom_richtext(aes(SocialSent, word,
    label = word_lab,
    hjust = hjust_s
  ),
  size = 2.3, color = "dimgrey", vjust = "middle",
  fill = NA, label.color = NA,
  label.padding = grid::unit(rep(2, 4), "pt")
  ) +
  scale_fill_gradient2() +
  coord_cartesian(xlim = c(-1.7, 1.9)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 9.5, color = "black", face = "bold"),
    axis.text.x = element_text(size = 9, color = "dimgrey"),
    axis.ticks.x = element_line(color = "dimgrey"),
    axis.ticks.length.x = unit(1, "mm")
  ) +
  scale_x_continuous(breaks = c(-1, 0, 1)) +
  labs(subtitle = "      i) SocialSent (2016)\n")
# +
#   annotate(
#     geom = "segment", y = 23, yend = 46, x = -1.65, xend = -1.65,
#     arrow = arrow(
#       ends = "both", type = "closed",
#       length = unit(2, "mm")
#     ), color = "dimgrey"
#   ) +
#   annotate(
#     geom = "richtext", x = -1.5, y = 34.5,
#     angle = 90, fill = NA, label.color = NA,
#     label.padding = grid::unit(rep(0, 4), "pt"),
#     color = "dimgrey", size = 3,
#     label = "Ranked according to our rating"
#   ) +
#   annotate(
#     geom = "text", x = -1.55, y = 47, label = "positive",
#     color = "dimgrey", size = 3, vjust = "bottom", hjust = "center"
#   ) +
#   annotate(
#     geom = "text", x = -1.55, y = 22, label = "negative",
#     color = "dimgrey", size = 3, vjust = "top", hjust = "center"
#   )

us <- by_word %>%
  group_by(word) %>%
  summarise(across(socialsent:us,
    list(mean = mean, sd = sd, n = length),
    .names = "{col}.{fn}"
  )) %>%
  mutate(
    word = fct_reorder(word, us.mean)
  ) %>%
  select(word, us.mean, us.n) %>%
  arrange(word) %>%
  mutate(hjust = case_when(
    us.mean > 0 ~ "right",
    TRUE ~ "left"
  )) %>%
  ggplot() +
  geom_col(aes(us.mean, word, fill = us.mean)) +
  geom_richtext(aes(0, word,
    label = word, hjust = hjust
  ),
  size = 2.3, color = "dimgrey", vjust = "middle",
  fill = NA, label.color = NA,
  label.padding = grid::unit(rep(2, 4), "pt")
  ) +
  scale_fill_gradient2() +
  coord_cartesian(xlim = c(-1.25, 1.25)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 9.5, color = "black", face = "bold"),
    axis.text.x = element_text(size = 9, color = "dimgrey"),
    axis.ticks.x = element_line(color = "dimgrey"),
    axis.ticks.length.x = unit(1, "mm")
  ) +
  scale_x_continuous(breaks = c(-1, 0, 1))

us_bar <- us + plot_annotation(
  title = "a) Average manually-assigned\n   sentiment score of sentences\n   containing common words",
  caption = "Average standardized score",
  theme = theme(
    plot.background = element_rect(
      colour = "black",
      size = 1
    ),
    plot.title = element_text(size = 12),
    plot.caption = element_text(
      hjust = .5,
      color = "dimgrey"
    )
  )
)


# put figures together
word_bars <- ss_comp + vader_comp + nlp_comp +
  plot_annotation(
    title = "b) Differences in sentiment from our ratings",
    caption = "Difference in standardized sentiment score from our rating",
    theme = theme(
      plot.background = element_rect(
        colour = "black",
        size = 1
      ),
      plot.title = element_text(size = 12),
      plot.caption = element_text(
        hjust = .5,
        color = "dimgrey"
      )
    )
  )

# save figure
ggsave("figures/word_bars.png", word_bars, height = 6.5, width = 8.75)
ggsave("figures/word_bars_us.png", us_bar, height = 6.5, width = 2.5)


#### FIGURE 1 ####
summary_ratings <- summary_ratings %>%
  mutate(
    mean_others = (socialsent + nlp + vader) / 3,
    dif_us = abs(mean_others - us),
    mad = (abs(socialsent - us) + abs(nlp - us) + abs(vader - us)) / 3
  )

to_compare <- summary_ratings %>%
  slice_min(mad, n = 3) %>%
  mutate(discrepant = FALSE) %>%
  bind_rows({
    summary_ratings %>%
      slice_max(mad, n = 5) %>%
      mutate(discrepant = TRUE)
  }) %>%
  select(article_sentence:us, discrepant, words) %>%
  mutate(
    wrapped_sentence = str_wrap(article_sentence, 60),
    wrapped_sentence = str_replace_all(wrapped_sentence, "\\n", "<br>")
  ) %>%
  arrange(desc(us))


compare_sentiment_poss <- possibly(compare_sentiment, 
                                   otherwise = tibble(.rows = 0))

phrases <- to_compare$article_sentence %>%
  str_split("\\;|\\,|( but )|( and )|( or )|( yet )|( so ) |( from )|( that )|( in )|( if )")

phrases <- tibble(
  phrase = phrases,
  sentence = to_compare$article_sentence
) %>%
  unnest_longer(phrase)

phrase_scores <- map_dfr(
  phrases$phrase,
  compare_sentiment_poss,
  socialsent_reg,
  stop_words
)
sentence_scores <- map_dfr(
  unique(phrases$sentence),
  compare_sentiment_poss,
  socialsent_reg,
  stop_words
)

phrases <- phrases %>%
  left_join(phrase_scores, by = c("phrase" = "sentence")) %>%
  left_join(sentence_scores,
    by = "sentence",
    suffix = c("_phrase", "_score")
  ) %>%
  mutate(
    nlp_phrase = parse_number(nlp_phrase),
    nlp_score = parse_number(nlp_score)
  ) %>%
  filter(!is.na(socialsent_score), !is.na(nlp_score), !is.na(vader_score))

nlp_breaks <- c(2, 2.99999)
socialsent_breaks <- c(.15, .25)
vader_breaks <- c(0, .00001)

color_pos <- function(word, color = "#3A3A98") {
  str_glue("<span style='color:{color};font-weight:bold'>{word}</span>")
}
color_neg <- function(word, color = "#A85C55") {
  str_glue("<span style='color:{color};font-weight:bold'>{word}</span>")
}

vader_pos <- phrases %>%
  filter(vader_phrase > vader_breaks[2]) %>%
  pull(phrase) %>%
  str_c("(", ., ")", collapse = "|")
vader_neg <- phrases %>%
  filter(vader_phrase < vader_breaks[1]) %>%
  pull(phrase) %>%
  str_c("(", ., ")", collapse = "|")
vader_sentences <- unique(phrases$sentence) %>%
  str_replace_all(vader_pos, color_pos) %>%
  str_replace_all(vader_neg, color_neg)

socialsent_pos <- phrases %>%
  filter(socialsent_phrase > socialsent_breaks[2]) %>%
  pull(phrase) %>%
  str_c("(", ., ")", collapse = "|")
socialsent_neg <- phrases %>%
  filter(socialsent_phrase < socialsent_breaks[1]) %>%
  pull(phrase) %>%
  str_c("(", ., ")", collapse = "|")
socialsent_sentences <- unique(phrases$sentence) %>%
  str_replace_all(socialsent_pos, color_pos) %>%
  str_replace_all(socialsent_neg, color_neg)

nlp_pos <- phrases %>%
  filter(nlp_phrase > nlp_breaks[2]) %>%
  pull(phrase) %>%
  str_c("(", ., ")", collapse = "|")
nlp_neg <- phrases %>%
  filter(nlp_phrase < nlp_breaks[1]) %>%
  pull(phrase) %>%
  str_c("(", ., ")", collapse = "|")
nlp_sentences <- unique(phrases$sentence) %>%
  str_replace_all(nlp_pos, color_pos) %>%
  str_replace_all(nlp_neg, color_neg)

all_sentences <- phrases %>%
  filter(!duplicated(sentence)) %>%
  select(-ends_with("phrase")) %>%
  mutate(
    vader_sentence = vader_sentences,
    socialsent_sentence = socialsent_sentences,
    nlp_sentence = nlp_sentences
  )

replace_space <- function(words) {
  str_replace_all(words, "\\s", "<br>") %>%
    str_replace_all("\\<", "<br><") %>%
    str_replace_all("\\>", "><br>") %>%
    str_replace_all("(<br>)+", "<br>")
}

all_break_spots <- to_compare %>% 
  select(ends_with("sentence")) %>%
  right_join(all_sentences, by = c("article_sentence" = "sentence")) %>%
  pull(wrapped_sentence) %>%
  str_extract_all("(?:\\s+)?\\S*\\<br\\>\\S*(?:\\s+)?") 

end_br <- map(all_break_spots, ~str_replace_all(.x, ".+\\<", "><")) %>%
  map_chr(~ {
    .x %>%
      str_trim() %>%
      str_remove_all("\\<br\\>") %>%
      str_c("(\\", ., ")", collapse = "|")
  })

start_br <- map(all_break_spots, ~str_replace_all(.x, "\\>.+", "><")) %>%
  map_chr(~ {
    .x %>%
      str_trim() %>%
      str_replace_all("\\<br\\>", "\\\\") %>%
      str_c("(", ., ")", collapse = "|")
  })


all_break_spots <- all_break_spots %>%
  map_chr(~ {
    .x %>%
      str_trim() %>%
      str_replace_all("\\<br\\>", " ") %>%
      str_c("(", ., ")", collapse = "|")
  })

all_break_spots <- str_c(all_break_spots, start_br, end_br, sep = "|")

all_sentences <- all_sentences %>%
  left_join(
    select(summary_ratings, article_sentence, mean_us),
    by = c("sentence" = "article_sentence")) %>%
  mutate(
    break_spots = all_break_spots,
    n_breaks = str_count(break_spots, "\\s") + 1,
    n_breaks = ifelse(n_breaks == 1, 2, n_breaks),
    y = lag(cumsum(n_breaks), default = 1),
    vader_sentence = str_replace_all(vader_sentence, break_spots, replace_space),
    socialsent_sentence = str_replace_all(socialsent_sentence, break_spots, replace_space),
    nlp_sentence = str_replace_all(nlp_sentence, break_spots, replace_space)
  )

long_sentences <- all_sentences %>%
  select(-break_spots, -n_breaks) %>%
  rename(sentence_orig = sentence) %>%
  pivot_longer(
    cols = -c(sentence_orig, y, mean_us), names_to = c("algorithm", ".value"),
    names_sep = "_"
  ) %>%
  mutate(sentence = paste0(sentence, " (", round(mean_us, 2), ")"))

# some manual editing to fix spacing
# write_csv(long_sentences, "data/sentences_to_plot-new.csv")
long_sentences <- read_csv("data/sentences_to_plot.csv") %>%
  mutate(algorithm = fct_relevel(algorithm, "socialsent", "vader", "nlp"))

our_scores <- summary_ratings %>%
  select(article_sentence, mean_us) %>%
  right_join(long_sentences, by = c("article_sentence" = "sentence_orig"))

sentence_plot <- ggplot(long_sentences) +
  geom_richtext(aes(
    x = 1, y = -y,
    label = sentence
  ),
  hjust = "left", vjust = "top", fill = NA, 
    label.padding = grid::unit(rep(1, 4), "pt"),
  label.color = NA
  ) +
  facet_grid(cols = vars(algorithm), 
             labeller = labeller(algorithm = c(nlp = "c) CoreNLP (2014)", 
                                               socialsent = "a) SocialSent (2016)", 
                                               vader = "b) VADER (2014)"))) +
  coord_cartesian(xlim = c(-3, 24), ylim = c(-(max(all_sentences$y) + 4), -1)) +
  theme_void() + 
  theme(strip.text.x = element_text(
        size = 12, color = "black", face = "bold"
        )) +
  geom_text(data = our_scores, x = -1.5, aes(y = -y - 0.5, label = round(score, 2)))


just_arrow <- ggplot() +
  ylim(10, 60) + xlim(-20, -1.4) +
  theme_void() + 
  annotate(
    geom = "segment", y = 23, yend = 46, 
    x = -1.65, xend = -1.65,
    arrow = arrow(
      ends = "both", type = "closed",
      length = unit(2, "mm")
    ), color = "dimgrey"
  ) +
  annotate(
    geom = "richtext", x = -1.5, y = 34.5,
    angle = 90, fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    color = "dimgrey", size = 3,
    label = "Ranked according to our rating"
  ) +
  annotate(
    geom = "text", x = -1.55, y = 47, label = "positive",
    color = "dimgrey", size = 3, vjust = "bottom", hjust = "center"
  ) +
  annotate(
    geom = "text", x = -1.55, y = 22, label = "negative",
    color = "dimgrey", size = 3, vjust = "top", hjust = "center"
  )

just_arrow + sentence_plot + 
  theme(plot.margin = unit(c(5, 0, 5, 0), "pt"))

ggsave("figures/sentences.png",
       height = 5.15, width = 31)
