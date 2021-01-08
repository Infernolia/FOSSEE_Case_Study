#Removing Stop Words
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords
import string

stop_list = stopwords.words('english')
stop_list += list(string.punctuation)
stop_list += ['br', '.<', '..', '...', '``', "''", '--']

from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
from sklearn.naive_bayes import MultinomialNB

clf = Pipeline([
    ('vect', CountVectorizer(stop_words=stop_list)),
    ('tfidf', TfidfTransformer()),
    ('classifier', MultinomialNB()),
    ])

nb_clf = clf.fit(X_train, y_train)
print('Accuracy:', nb_clf.score(X_test, y_test))

# data['Helpfulness'] = nb_clf.predict(data['lemm_processed_text'])