from socialsent.polarity_induction_methods import random_walk
from socialsent.representations.representation_factory import create_representation
import pandas as pd
from nltk.tokenize import word_tokenize
import pickle

pos_seeds = ["good", "nice", "excellent", "positive", "fortunate", "correct", "superior"]
neg_seeds = ["bad", "terrible", "poor", "negative", "unfortunate", "wrong", "inferior"]

filename = "data/articles_scored.csv"
coronavirus_jail_df = pd.read_csv(filename)
words = [word_tokenize(str(article)) for article in coronavirus_jail_df["text_clean"]]
flat_words = [word for article in words for word in article]
unique_words = list(set(flat_words))

embeddings = create_representation("word2vec", 
        "socialsent-master/GoogleNews-vectors-negative300.bin", unique_words,
        set(unique_words).union(pos_seeds).union(neg_seeds))

polarities = random_walk(embeddings, pos_seeds, neg_seeds, beta = 0.8, nn = 15,
        sym = True, arccos = True)

pickle.dump(polarities, open("data/social_sent_reg_scores.pkl", "wb"))

pos_seeds2 = ["justice", "protect", "trust", "rights", "peace", "public", "respect"] 
neg_seeds2 = ["chaos", "incident", "violent", "attack", "danger", "risk", "offend"]

embeddings2 = create_representation("word2vec", 
        "socialsent-master/GoogleNews-vectors-negative300.bin", unique_words,
        set(unique_words).union(pos_seeds2).union(neg_seeds2))

polarities2 = random_walk(embeddings2, pos_seeds2, neg_seeds2, beta = 0.8, nn = 15,
        sym = True, arccos = True)

pickle.dump(polarities2, open("data/social_sent_new_scores.pkl", "wb"))
