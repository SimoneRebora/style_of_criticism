import os
import gensim
import urllib.request
import os.path
import pandas
import numpy as np

#%% First step: define the word embedding file
#   if not present, it is downloaded automatically (it might take a while, as it is a 2Gb file)

filename = 'wiki.it.vec'
download_link = "https://s3-us-west-1.amazonaws.com/fasttext-vectors/wiki.it.vec"

if not os.path.isfile(filename):
    print("Model not present!!\nDownload starts now...\n(it might take a while, as it is a 2Gb file...)")
    urllib.request.urlretrieve(download_link, filename)

#%% Upload the model
italian_model = gensim.models.KeyedVectors.load_word2vec_format(filename, binary=False)


#%% Read the  Mental Imagery lexicon

os.chdir("./lexicons")

lexicon_title = "Lexicon_Mental_Imagery.csv"
output_file = "Lexicon_Mental_Imagery_ext.csv"

lexicon = pandas.read_csv(lexicon_title)

#%% Use the model to expand the lexicon (top 20 most similar words)

final_lexicon = []

for test_word in lexicon.Italiano:
   if test_word in italian_model.wv.vocab:
       new_vector = []
       w_vec = italian_model.similar_by_vector(italian_model.wv[test_word], topn=20)
       for i in range(0, len(w_vec)):
           new_vector += [w_vec[i][0]]
           new_vector[i] = new_vector[i].replace("»", "")
           new_vector[i] = new_vector[i].replace(",", "")
   final_lexicon += new_vector

#%% Save in a new csv file

final_lexicon = list(set(final_lexicon))

lexicon = pandas.DataFrame(list(zip(final_lexicon)),
              columns=['word'])
    
lexicon.to_csv(output_file)

#%% Now, the same with the Knoop lexicon

lexicon_title = "Lexicon_Knoop.csv"
output_file = "Lexicon_Knoop_ext.csv"

lexicon = pandas.read_csv(lexicon_title)

def is_nan(x):
    return (x is np.nan or x != x)

for index in range(0, len(lexicon)):
   if is_nan(lexicon.Emotional[index]):
       lexicon = lexicon.drop(index)

#%% Use the model to expand the lexicon (top 20 most similar words)

final_lexicon = []

for test_word in lexicon.ITALIANO:
   if test_word in italian_model.wv.vocab:
       new_vector = []
       w_vec = italian_model.similar_by_vector(italian_model.wv[test_word], topn=20)
       for i in range(0, len(w_vec)):
           new_vector += [w_vec[i][0]]
           new_vector[i] = new_vector[i].replace("»", "")
           new_vector[i] = new_vector[i].replace(",", "")
   final_lexicon += new_vector

#%% Save in a new csv file

final_lexicon = list(set(final_lexicon))

lexicon = pandas.DataFrame(list(zip(final_lexicon)),
              columns=['word'])
    
lexicon.to_csv(output_file)
