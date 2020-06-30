from pathlib import Path
import pathlib
import json
from bs4 import BeautifulSoup as bs
import pandas as pd
import copy
from nltk.collocations import BigramCollocationFinder
import string
from zhon import hanzi
import numpy as np
from scipy import stats
import nltk
from nltk.collocations import * 
import pickle

def string_to_token_list(text):
    token_list = []
    for item in bs(text, 'xml').find(type='body').string.split('\n'):
        if item != '':
            token_list.append(item.split('\t')[0])
    return token_list

with open('stopword.txt', 'r') as f:
    text = f.read()
stopword = set(text.split('\n'))|set(hanzi.punctuation)|set(string.punctuation)

def group_tokenize(group):
    group_list =[]
    for item in group['無POS斷詞後內文']:
        group_list.extend(item)
    group_list_no_sw =[]
    for item in group_list:
        if item.upper() not in stopword:
            group_list_no_sw.append(item.upper())
    return group_list_no_sw

def generate_compare_list(g1, g2):
    # g1 = df[(df['版'] == 'BabyMother') & (df['高影響力'] == True)]
    g1_count = {}
    for key in group_tokenize(g1):
        g1_count[key] = g1_count.get(key, 0) + 1

    # g2 = df[(df['版'] == 'BabyMother') & (df['高影響力'] == False)]
    g2_count = {}
    for key in group_tokenize(g2):
        g2_count[key] = g2_count.get(key, 0) + 1

    compare_count = {}
    for key in g1_count:
        compare_count[key] = g1_count[key]/len(g1)
    for key in g2_count:
        compare_count[key] = compare_count.get(key, 0) - g2_count[key]/len(g2)

    compare_list = []
    for key in compare_count:
        compare_list.append((key, compare_count[key]))
    return compare_list

def compare_board_ranked(df, board):
    compare_list = generate_compare_list(df[(df['版'] == board) & (df['高影響力'] == True)], df[(df['版'] == board) & (df['高影響力'] == False)])
    compare_top30 = []
    for item in sorted(compare_list, key = lambda comp: comp[1], reverse = True)[:30]:
        compare_top30.append(item[0])
    compare_last30 = []
    for item in sorted(compare_list, key = lambda comp: comp[1])[:30]:
        compare_last30.append(item[0])
    return [board, compare_top30, compare_last30]

# df preprocessing
df = pd.read_pickle('data.pkl')
df['無POS斷詞後內文'] = df['斷詞後內文'].apply(string_to_token_list)
df['token數'] = df['無POS斷詞後內文'].apply(len)

def repetition(df):
    
    df_repitition = pd.DataFrame([compare_board_ranked(df, 'BabyMother'),
                              compare_board_ranked(df, 'Boy-Girl'),
                              compare_board_ranked(df, 'WomenTalk'),
                              compare_board_ranked(df, 'marriage')], columns = ['板', '高影響力', '非高影響力'])
    
    return df_repitition

def length_of_posts(df):
    df_length1 = df.groupby(['版', '高影響力'])['token數'].mean()
    df_length2 = df.groupby('高影響力')['token數'].mean()
    df_length = df_length1.append(df_length2)
    return df_length

def collocation(df):
    def find_collocation_bigram(board, boo, size, item, freq, min_w = 0):
        text = group_tokenize(df[(df['版'] == board) & (df['高影響力'] == boo)])
        corpus = nltk.Text(text) # "text" is a list of tokens 
        bigram_measures = nltk.collocations.BigramAssocMeasures() 
        bi_finder = BigramCollocationFinder.from_words(corpus, window_size = size) 
        # Only bigrams that appear n+ times 
        bi_finder.apply_freq_filter(freq) 
        # Only the ones containing my word 
        filter = lambda *w: item not in w 
        bi_finder.apply_ngram_filter(filter) 
        # print(bi_finder.nbest(bigram_measures.likelihood_ratio, 20))
        collo_list = []
        for i in bi_finder.score_ngrams(bigram_measures.likelihood_ratio):
            if i[1] >= 0:
                collo_list.append(i)
        return collo_list
    
    list_collocation = []
    for board in ['BabyMother', 'Boy-Girl', 'WomenTalk', 'marriage']:
        for item in compare_board_ranked(df, board)[1]:
            list_collocation.append((board, True, item, 
                                    len(find_collocation_bigram(board, True, 2, item, 2))))
        for item in compare_board_ranked(df, board)[2]:
            list_collocation.append((board, False, item, 
                                len(find_collocation_bigram(board, False, 2, item, 2))))
    df_collocation = pd.DataFrame(list_collocation, columns = ['board', 'group', 'repetition', 'collocation'])
    df_collocation['group'] = df_collocation['group'].astype(int)
    return df_collocation

