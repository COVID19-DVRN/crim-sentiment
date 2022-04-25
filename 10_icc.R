library(tidyverse)

nyt_dat <- read_tsv("https://raw.githubusercontent.com/totalgood/nlpia/master/src/nlpia/data/hutto_ICWSM_2014/nytEditorialSnippets_anonDataRatings.txt", 
                    col_names = FALSE) %>% 
  mutate(X4 = str_remove_all(X4, "[\\]\\[]")) %>% 
  separate(X4, sep = ",", into = letters[1:20]) %>% 
  select(a:t) %>% 
  mutate(across(everything(), parse_number))

ours <- map(1:5,~read_csv(str_glue("https://media.githubusercontent.com/media/COVID19-DVRN/crim-sentiment/main/data/ratings{.x}.csv"))) %>% 
  reduce(full_join, by = "sentence") %>% 
  distinct() %>% 
  select(-sentence)

irr::icc(nyt_dat)$value
irr::icc(ours)$value
