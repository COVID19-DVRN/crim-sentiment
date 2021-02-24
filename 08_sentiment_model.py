#### Packages ####

# Auxiliary
import pandas as pd
import numpy as np
from sklearn.base import TransformerMixin
from joblib import dump

# Validation
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import KFold

# Feature extraction
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfTransformer

# Models
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor


def norm(a, b):
  """
  Normalize dataset 'a' by mean and standard deviation of 'b'.
  Used, for instance, to normalize test data by using parameters derived from training data.
  """
  return (a - np.mean(b)) / np.std(b)


def fit_models(ratings, systems):
  X = ratings['sentence']
  y = ratings['mean_us']
  
  errors_predict = []
  errors_systems = []
  every_error_regress = []
  every_error_rf = []
  all_errors_systems = []
  
  kf = KFold(n_splits=10)
  vect = CountVectorizer(binary=True, 
      )
  
  for train_index, test_index in kf.split(X):
      
    X_train, X_test = X.iloc[train_index], X.iloc[test_index]
    y_train, y_test = y.iloc[train_index], y.iloc[test_index]
    
    # Fit vectorizer and transform X train, then transform X test
    X_train_vect = vect.fit_transform(X_train)
    X_test_vect = vect.transform(X_test)
  
    # # Note: this section may make the explanation more complicated, but it does slightly improve performance
    # tfidf_transformer=TfidfTransformer(smooth_idf=True,use_idf=True) 
    # X_train_vect = tfidf_transformer.fit_transform(X_train_vect)
    # X_test_vect = tfidf_transformer.transform(X_test_vect)
  
    # Normalize test and train *actual* ratings
    y_test = norm(y_test, y_train)
    y_train = norm(y_train, y_train)
    
    ## REGRESSION
    regress = LinearRegression()
    regress.fit(X_train_vect, y_train)
    y_pred = regress.predict(X_test_vect)
    # Prediction error
    error = np.abs(y_pred - y_test)
    every_error_regress.append(error)
    error_mean_regress = np.mean(error)
  
    ## RANDOM FOREST
    rf = RandomForestRegressor()
    rf.fit(X_train_vect, y_train)
    y_pred = rf.predict(X_test_vect)
    # Prediction error
    error = np.abs(y_pred - y_test)
    every_error_rf.append(error)
    error_mean_rf = np.mean(error)
  
    # Data compilation
    errors_predict.append([error_mean_regress, error_mean_rf])
  
    # Model errors
    error_systems = []
    every_error_system = []
  
    for system in systems:
      # Normalize test/train ratings of the other systems
      y_train_system = ratings[system][train_index]
      y_test_system = ratings[system][test_index]
      y_test_system = norm(y_test_system, y_train_system)
      y_train_system = norm(y_train_system, y_train_system)
  
      # Prediction error
      error = np.abs(y_test - y_test_system)
      every_error_system.append(error)
      error_mean = np.mean(error)
      error_systems.append(error_mean)
    
    # Data compilation
    all_errors_systems.append(every_error_system)
    errors_systems.append(error_systems)
      
  return np.array(errors_predict), np.array(errors_systems), every_error_regress, every_error_rf, all_errors_systems


