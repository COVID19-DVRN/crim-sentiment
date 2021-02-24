library(tidyverse)
library(patchwork)
library(ggtext)

summary_ratings <- read_rds("data/summary_ratings.rds")

# find 100 most-used words
all_words <- pull(summary_ratings, words) %>%
  flatten_chr()

rm_words <- c("january", "february", "march", "april", "may", 
  "june", "july", "august", "september", "october",
  "november", "december", "monday", "tuesday", "wednesday",
  "thursday", "friday", "saturday", "sunday", 
  "one", "two", "three", "four", "five", "six", 
  "seven", "eight", "nine", "ten", 
  "could", "also", "across", "would", "since", 
  "said", "say", "says", "according", "many", "among", "19",
  "even", "like", "ohio", "us", "within", "accord", "new", "year",
  "get", "ask", "call", "go", "less", "little", "due", "give", 
  "day", "week", "still", "back", "make", "much", "come", "take", "see")

stop_words_str <- c('few', 'most', 'he', 'can', "you'd", 'themselves', 'be', "shan't",
                    'very', 'doing', 'ma', 'just', "mustn't", 'won', 'below', 'only', 'at', 'my', 'his',
                    'again', 'while', "mightn't", "you've", 'some', 'your', "that'll", "she's", 's', 'shouldn', 
                    'been', 'does', 'needn', 'its', 'who', 'do', 'down', 'no', 'd', 'until', 'all', 'then',
                    'yourself', 'am', "you're", 're', 'doesn', "needn't", 'herself', 'against', 'after', 'so',
                    'what', 'ourselves', "aren't", 'if', "you'll", 'but', 'when', 'any', "shouldn't", "won't",
                    'should', 'as', 'above', "couldn't", 'you', 'hers', 'had', 'don', 'is', 'of', 'how',
                    'where', 'those', 'this', "wasn't", 'to', 'there', 'by', 'own', 'being', 'yourselves',
                    'for', "wouldn't", 'y', 'they', 'each', 'such', "doesn't", 'myself', 'aren', 'him',
                    "weren't", 'll', 'nor', 'wouldn', 'than', 'both', 'me', 'i', 'are', 'through', 'ours',
                    'during', 'up', 'a', 'between', "should've", 'wasn', 'more', 'she', 'out', 'it', "hadn't",
                    'haven', 'that', 'we', 'not', 'in', 'ain', 'before', 'them', 'here', "isn't", "didn't",
                    'over', 'weren', 'did', 'the', "it's", 'isn', 'didn', 'under', "haven't", 'off', "don't",
                    'hadn', 'about', 'whom', 'from', 'her', 'an', 'same', 'having', 'have', 'now', 'himself',
                    'were', "hasn't", 't', 'yours', 'why', 'o', 'theirs', 'and', 'or', 'on', 'has', 'mustn',
                    'mightn', 'too', 'because', 'will', 'm', 'their', 'further', 've', 'was', 'couldn', 'our',
                    'itself', 'which', 'into', 'once', 'shan', 'with', 'these', 'other', 'hasn')

top_words <- tibble(words = all_words) %>%
  count(words) %>%
  filter(!words %in% stop_words_str) %>%
  filter(!words %in% rm_words) %>%
  filter(!words %in% c("-", ",", ".", "’", "``", "''", "(", ")", ":", "'re", "'s")) %>%
  count(words, wt = n) %>%
  filter(n > 10) %>%
  arrange(desc(n))

# create a logical variable for each of those words
summary_ratings <- summary_ratings %>%
  rename(article_sentence = sentence) %>%
  mutate(any_words = map(words, ~top_words$words %in% .))

summary_ratings_wide <- transpose(summary_ratings$any_words) %>%
  set_names(top_words$words) %>%
  as_tibble() %>%
  mutate_all(unlist) %>%
  bind_cols(summary_ratings)

# make long dataset with words in rows
by_word <- summary_ratings_wide %>%
  pivot_longer(-any_of(names(summary_ratings)),
               names_to = "word", values_to = "in_sentence") %>%
  filter(in_sentence)

# average across the words
word_summaries <- by_word %>%
  group_by(word) %>%
  summarise(across(socialsent:us, 
                   list(mean = mean, sd = sd, n = length),
                   .names = "{col}.{fn}")) %>%
  mutate(word = fct_reorder(word, us.mean)) %>%
  pivot_longer(-c(word),
               names_pattern = "(.*)\\.(.*)",
               names_to = c("method", ".value")) %>%
  mutate(method = fct_relevel(method, "us"),
         method = fct_rev(method))

crim_words <- c("release", "detainee", 
                "incarcerate", "sheriff", 
                "jail", "federal", "inmate",
                "correction", "prisoner",
                "prison", "justice", "police", "officer",
                "sentence", "detention", "facility", "center")

word_us_ref <- by_word %>%
  group_by(word) %>%
  summarise(across(socialsent:us, 
                   list(mean = mean, sd = sd, n = length),
                   .names = "{col}.{fn}")) %>%
  mutate(word = fct_reorder(word, us.mean),
         VADER = vader.mean - us.mean,
         SocialSent = socialsent.mean - us.mean,
         CoreNLP = nlp.mean - us.mean) %>%
  select(word, VADER, SocialSent, CoreNLP) %>%
  mutate(discrepant = VADER * SocialSent < 0,
         crim = word %in% crim_words,
         hjust_v = ifelse(VADER < 0, "right", "left"),
         hjust_s = ifelse(SocialSent < 0, "right", "left"),
         word_lab = ifelse(discrepant, str_glue("{word}*"), str_glue("{word}")),
         word_lab = ifelse(crim, str_glue(" **{word_lab}** "), str_glue(" {word_lab} ")))

nlp_comp <- ggplot(word_us_ref) +
  geom_col(aes(CoreNLP, word, fill = CoreNLP)) +
  geom_richtext(aes(CoreNLP, word, label = word_lab,
                    hjust = hjust_v), 
            size = 2.3, color = "dimgrey", vjust = "middle",
            fill = NA, label.color = NA,
            label.padding = grid::unit(rep(2, 4), "pt")) +
  scale_fill_gradient2() +
  coord_cartesian(xlim = c(-1.35, 1.7)) +
  theme_void() +
  theme(legend.position = "none", 
        plot.subtitle = element_text(size = 9, color = "dimgrey"),
        axis.text.x = element_text(size = 9, color = "dimgrey"),
        axis.ticks.x = element_line(color = "dimgrey"),
        axis.ticks.length.x = unit(1, "mm")) +
    scale_x_continuous(breaks = c(-1, 0, 1)) +
  labs(subtitle = "      c) CoreNLP (2014)\n")

vader_comp <- ggplot(word_us_ref) +
  geom_col(aes(VADER, word, fill = VADER)) +
  geom_richtext(aes(VADER, word, label = word_lab,
                    hjust = hjust_v), 
            size = 2.3, color = "dimgrey", vjust = "middle",
            fill = NA, label.color = NA,
            label.padding = grid::unit(rep(2, 4), "pt")) +
  scale_fill_gradient2() +
  coord_cartesian(xlim = c(-1.35, 1.7)) +
  theme_void() +
  theme(legend.position = "none", 
        plot.subtitle = element_text(size = 9, color = "dimgrey"),
        axis.text.x = element_text(size = 9, color = "dimgrey"),
        axis.ticks.x = element_line(color = "dimgrey"),
        axis.ticks.length.x = unit(1, "mm")) +
    scale_x_continuous(breaks = c(-1, 0, 1)) +
  labs(subtitle = "      b) VADER (2014)\n")

ss_comp <- ggplot(word_us_ref) +
  geom_col(aes(SocialSent, word, fill = SocialSent)) +
  geom_richtext(aes(SocialSent, word, label = word_lab,
                    hjust = hjust_s), 
            size = 2.3, color = "dimgrey", vjust = "middle",
            fill = NA, label.color = NA,
            label.padding = grid::unit(rep(2, 4), "pt")) +
  scale_fill_gradient2() +
  coord_cartesian(xlim = c(-1.35, 1.35)) +
  theme_void() +
  theme(legend.position = "none", 
        plot.subtitle = element_text(size = 9, color = "dimgrey"),
        axis.text.x = element_text(size = 9, color = "dimgrey"),
        axis.ticks.x = element_line(color = "dimgrey"),
        axis.ticks.length.x = unit(1, "mm")) +
  scale_x_continuous(breaks = c(-1, 0, 1)) +
  labs(subtitle = "      a) SocialSent (2016)\n") +
  annotate(geom = "segment", y = 23, yend = 46, x = -1.2, xend = -1.2,
           arrow = arrow(ends = "both", type = "closed", 
                         length = unit(2, "mm")), color = "dimgrey") +
  annotate(geom = "richtext", x = -1.13, y = 34.5, 
           angle = 90, fill = NA, label.color = NA,
           label.padding = grid::unit(rep(0, 4), "pt"),
           color = "dimgrey", size = 3,
           label = "Ranked according to our rating") +
  annotate(geom = "text", x = -1.2, y = 47, label = "positive", 
           color = "dimgrey", size = 3, vjust = "bottom", hjust = "center") +
  annotate(geom = "text", x = -1.2, y = 22, label = "negative", 
           color = "dimgrey", size = 3, vjust = "top", hjust = "center")

word_bars <- ss_comp + vader_comp + nlp_comp +
  plot_annotation(caption = "Difference in standardized sentiment score from our rating",
                  theme = theme(plot.caption = element_text(hjust = .5, 
                                                            color = "dimgrey")))

ggsave("figures/word_bars.png", word_bars, height = 6, width = 8.25)





discrepant <- c('Twenty-one other inmates have tested negative.',
                '"I find it absolutely terrifying."',
                'Multiple defense attorneys wondered if the move may be a silver lining to the fatal virus.',
                '"These are non-violent, non-serious charges and people who do not have a serious or violent record."',
                '"I Walter ‘Arkie’ Barton, am innocent, and they are executing an innocent man!!"')

consistent <- c('On April 10, he was formally arraigned via phone conference, according to attorneys and court records, and was released later that day.',
                'In Mexico, there are 19 high security federal prisons with about 17,000 inmates and 309 state prisons with about 176,000 inmates, according to official figures, and the country’s prison population is second in Latin America only to Brazil’s.',
                'Some are released on bail before they are moved into the general population, Jahner said.',
                'State officials also provided the latest details about the spread of COVID-19 in Illinois.',
                'Cory Schulte, Riverbend’s executive director, didn’t immediately reply to a phone message Wednesday seeking comment.')

some_sentences <- unstandardized %>%
  filter(sentence %in% sentences) %>%
  mutate(words = map(sentence, ~str_split(.x, " ", simplify = TRUE)),
         vals = map(words, ~map_dfr(.x, compare_sentiment, socialsent_reg, stop_words)),
         ss_words = imap(words, ~.x[is.finite(vals[[.y]]$socialsent)]),
         vader_words = imap(words, ~.x[vals[[.y]]$vader != 0]),
         ss_values = map(vals, ~.x$socialsent[is.finite(.x$socialsent)]),
         vader_values = map(vals, ~.x$vader[.x$vader != 0]))

ss_all <- tibble(word = flatten_chr(some_sentences$ss_words),
                 value = flatten_dbl(some_sentences$ss_values))
vader_all <- tibble(word = flatten_chr(some_sentences$vader_words),
                 value = flatten_dbl(some_sentences$vader_values))

all_words <- full_join(ss_all, vader_all, by = "word", suffix = c("_ss", "_vader")) %>%
  mutate(value_vader = ifelse(is.na(value_vader), 0, value_vader),
         color_vader = ifelse(value_vader > 0, "#3A3A98", ifelse(value_vader < 0, "#A85C55", "dimgrey")),
         color_ss = ifelse(value_vader >= .35, "#3A3A98", ifelse(value_vader < .2, "#A85C55", "dimgrey"))) %>%
  distinct()

color_word <- function(word, color){
  str_glue("<span style='color:{color}'>{word}</span>")
}

sentences_vader <- str_wrap(some_sentences$sentence, width = 65)
for (j in seq_along(sentences_vader)) {
  for (i in seq_along(all_words$word)) {
          sentences_vader[j] <- gsub(all_words$word[i], 
                               color_word(all_words$word[i], all_words$color_vader[i]), 
                               sentences_vader[j],
                               ignore.case = TRUE)
    
  }
}

sentences_ss <- str_wrap(some_sentences$sentence, width = 65)
for (j in seq_along(sentences_ss)) {
  for (i in seq_along(all_words$word)) {
          sentences_ss[j] <- gsub(all_words$word[i], 
                               color_word(all_words$word[i], all_words$color_ss[i]), 
                               sentences_ss[j],
                               ignore.case = TRUE)
    
  }
}
sentences_to_plot <- tibble(sentence = str_replace_all(c(sentences_vader,sentences_ss), "\\n", "<br>"),
                            y = c(seq_along(sentences_vader), seq_along(sentences_ss)),
                            x = c(rep("VADER", length(sentences_vader)), rep("SocialSent", length(sentences_ss))))

ggplot(sentences_to_plot) +
  geom_richtext(aes(x = 1, y = y, label = sentence),
                hjust = "left", fill = NA, label.color = NA,
                label.padding = grid::unit(rep(1, 4), "pt")) +
  coord_cartesian(xlim = c(1, 3)) +
  facet_grid(rows = vars(y), cols = vars(x), scales = "free") +
  theme_void()






ggplot(word_us_ref) +
  geom_col(aes(VADER, word, fill = VADER)) +
  geom_text(aes(0, word, label = word)) +
  scale_fill_gradient2() +
  theme(panel.grid = element_blank())

ggplot(filter(word_summaries, method %in% c("us", "socialsent", "vader"))) +
  geom_col(aes(y = word, x = mean, fill = method,
               alpha = method == "us"),
           position = position_dodge2(padding = 0, width = .2)) +
    scale_alpha_manual(values = c(.6, 1), guide = NULL) +
  scale_fill_manual(values = c("#a6cee3", "#b2df8a", "#fb9a99"),
                    labels = c("VADER", "SocialSent", "Our group"),
                    name = "Algorithm") +
  facet_grid(rows = vars(word), 
             scales = "free", as.table = FALSE) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines")) +
  ggtitle("Average sentence sentiment of sentences\ncontaining these words")

ranks <- by_word %>%
  group_by(word) %>%
  summarise(across(socialsent:us, mean)) %>%
  mutate(across(socialsent:us, row_number)) %>%
  select(word, socialsent, vader, us) %>%
  pivot_longer(c(socialsent, vader), names_to = "comparison",
               values_to = "rank") %>%
  mutate(diff = case_when(
           us == rank ~ "same",
           us < rank ~ "more negative",
           us > rank ~ "more positive"
         )) %>%
  arrange(-us) %>%
  mutate(col = fct_relevel(diff, "more negative", "same", "more positive"))

vader <- filter(ranks, comparison == "vader")
ss <- filter(ranks, comparison == "socialsent")

to_plot_vader <- tibble(x = rep(c(0, 1), each = nrow(vader)),
                          y = rep(1:nrow(vader), 2),
                  x1 = ifelse(x == 0, 0.1, .9),
                  lab = c(pull(arrange(vader, us), word), 
                          pull(arrange(vader, rank), word))) %>%
  left_join(vader, by = c("lab" = "word"))

to_plot_ss <- tibble(x = rep(c(0, -1), each = nrow(ss)),
                          y = rep(1:nrow(ss), 2),
                  x1 = ifelse(x == 0, -0.1, -.9),
                  lab = c(pull(arrange(ss, us), word), 
                          pull(arrange(ss, rank), word))) %>%
  left_join(ss, by = c("lab" = "word"))

to_plot <- bind_rows(to_plot_vader, to_plot_ss) %>%
  mutate(g = paste0(lab, comparison))

ggplot(to_plot, aes(x = x, y = y, group = g, label = lab)) +
  geom_path(aes(x = x1, col = col),
            size = 1) +
  geom_text(size = 4) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

# save figures
# ggsave("sentiment_corr.png", p_corr, height = 8, width = 8)
# ggsave("sentiment_fill.png", p_fill, height = 10, width = 6)
# ggsave("sentiment_tri.png", p_common, height = 8, width = 12)
# ggsave("sentiment_from_avg.png", p_dist, height = 15, width = 6)
