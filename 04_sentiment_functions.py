import pandas as pd
import pickle
import numpy as np

from nltk import word_tokenize
from nltk.corpus import stopwords
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

# in terminal: stanford-corenlp-4.1.0 % java -mx4g -cp "*" edu.stanford.nlp.pipeline.StanfordCoreNLPServer -annotators "tokenize,ssplit,pos,lemma,parse,sentiment" -port 9000 -timeout 30000
# ctrl+c in terminal afterward to close
from pycorenlp import StanfordCoreNLP
nlp = StanfordCoreNLP('http://localhost:9000')

analyzer = SentimentIntensityAnalyzer()

stop_words = set(stopwords.words('english')) 
stop_words_str = list(stop_words)
socialsent_reg = pickle.load(open("data/social_sent_reg_scores.pkl", "rb"))

def try_word_pol(itr, polarities):
    for elem in itr:
        try:
             yield polarities[elem]
        except KeyError:
             pass

def socialsent(sentence, sentiments, stop_words):
  new_words = word_tokenize(sentence)
  # keep non-stop words
  keep_words = [w for w in new_words if not w.lower() in stop_words]
  # get polarity of each word based on "sentiments"
  pol_words = list(try_word_pol(keep_words, sentiments))
  if len(pol_words) > 0:
      return sum(pol_words)/len(new_words)
  else:
      return np.nan
      
def nlp_sentiment(sentence):
    result = nlp.annotate(sentence,
                       properties={
                           'annotators': 'sentiment',
                           'outputFormat': 'json',
                           'timeout': 3000,
                       })
    return [s["sentimentValue"] for s in result["sentences"]]
        

def compare_sentiment_other(sentence, socialsent_reg, stop_words):
    socialsent_reg_score = socialsent(sentence, socialsent_reg, stop_words)
    nlp_score = nlp_sentiment(sentence)
    vader_score = analyzer.polarity_scores(sentence)["compound"]
    scores = {"sentence": sentence, "socialsent":  socialsent_reg_score, "nlp": nlp_score[0], "vader": vader_score}
    return scores

