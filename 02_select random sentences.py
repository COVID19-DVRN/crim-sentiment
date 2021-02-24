# code to randomly select 1000 sentences from the mediacloud article downloads
import pandas as pd
import random
from nltk import sent_tokenize

filename = "data/articles_scored.csv"
coronavirus_jail_df = pd.read_csv(filename)
coronavirus_jail_df = coronavirus_jail_df[coronavirus_jail_df["publish_date"] <= "2020-05-25T00:00:00Z"]
articles = coronavirus_jail_df["text_clean"]
sentences = [sent_tokenize(str(article)) for article in articles]
flat_sentences = [sentence for article in sentences for sentence in article]
unique_sentences = list(set(flat_sentences))
len(unique_sentences)
test_sentences = random.choices(unique_sentences, k = 1000)
to_save = pd.DataFrame(test_sentences, columns = ["sentence"])
to_save.to_csv("data/sentences_to_compare.csv", index = False)
