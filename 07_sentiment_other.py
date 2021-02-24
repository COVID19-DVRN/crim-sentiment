import pandas as pd
import pickle
import numpy as np

from nltk import word_tokenize, pos_tag
from nltk.stem import WordNetLemmatizer
from nltk.corpus import stopwords, opinion_lexicon, wordnet as wn, sentiwordnet as swn
from afinn import Afinn

lemmatizer = WordNetLemmatizer()
afinn = Afinn()

stop_words = set(stopwords.words('english')) 
stop_words_str = list(stop_words)

def try_word_pol(itr, polarities):
    for elem in itr:
        try:
             yield polarities[elem]
        except KeyError:
             pass

# based off https://nlpforhackers.io/sentiment-analysis-intro/
def penn_to_wn(tag):
    """
    Convert between the PennTreebank tags to simple Wordnet tags
    """
    if tag.startswith('J'):
        return wn.ADJ
    elif tag.startswith('N'):
        return wn.NOUN
    elif tag.startswith('R'):
        return wn.ADV
    elif tag.startswith('V'):
        return wn.VERB
    return None

def swn_polarity(sentence):
    
    sentiment = 0.0
    tokens_count = 0

    tagged_sentence = pos_tag(word_tokenize(sentence))
    for word, tag in tagged_sentence:
       # include all in denominator for comparison
        tokens_count += 1 
        wn_tag = penn_to_wn(tag)
        if wn_tag not in (wn.NOUN, wn.ADJ, wn.ADV):
            continue
        lemma = lemmatizer.lemmatize(word, pos=wn_tag)
        if not lemma:
            continue
        synsets = wn.synsets(lemma, pos=wn_tag)
        if not synsets:
            continue
        # Take the first sense, the most common
        synset = synsets[0]
        swn_synset = swn.senti_synset(synset.name())
        sentiment += swn_synset.pos_score() - swn_synset.neg_score()
    
    if tokens_count > 0:
      # average sentiment
        return sentiment/tokens_count
    else:
        return np.nan

# simplified version from nltk
def liu_hu_lexicon(sentence):
    pos_words = 0
    neg_words = 0
    tokenized_sent = [word.lower() for word in word_tokenize(sentence)]
    y = []
    if len(tokenized_sent) > 0:
        for word in tokenized_sent:
            if word in opinion_lexicon.positive():
                pos_words += 1
                y.append(1) # positive
            elif word in opinion_lexicon.negative():
                neg_words += 1
                y.append(-1) # negative
            else:
                y.append(0) # neutral
        return sum(y)/len(tokenized_sent)
    else:
        return np.nan


def affin_standardized(sentence): 
    afinn_score = afinn.score(sentence)
    words = word_tokenize(sentence)
    if len(words) > 0:
      # average sentiment
        return afinn_score/len(words)
    else:
        return np.nan
        

def compare_sentiment_other(sentence):
    liu_hu_score = liu_hu_lexicon(sentence)
    swn_score = swn_polarity(sentence)
    afinn_score = affin_standardized(sentence)
    scores = {"sentence": sentence, "liu_hu":  liu_hu_score, "swn": swn_score, "afinn": afinn_score}
    return scores

