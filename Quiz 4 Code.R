# Load libraries
library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(devtools)
library(quanteda.corpora)
library(irr)

#Read the data into R
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176")
climate <- read_csv("WebNewsEnglishSnippets.2020.csv", col_names=F)
names(climate) <- c("date", "headline", "image", "url", "snippet")
climate

# Create a corpus for the headlines
corpus_climate <- corpus(climate, text_field = "headline")
corpus_climate

# Create a document feature matrix (dfm)
# Some common pre-processing
toks <- tokens(corpus_climate, remove_punct = TRUE, remove_numbers=TRUE)
# We are going to use a dictionary to start, so I won't stem (commenting stemming out)
# toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfm <- dfm(toks)

# We'll use the lexicoder sentiment dictionary that's in quanteda
head(data_dictionary_LSD2015)

# Select only the "negative" and "positive" categories
data_dictionary_LSD2015_pos_neg <- data_dictionary_LSD2015[1:2]

# Categorize each word into positive or negative and count them per document
toks_gov_dict <- tokens_lookup(toks, dictionary = data_dictionary_LSD2015_pos_neg)

# Create a dfm of the result
dfm_neg_pos <- dfm(toks_gov_dict)
dfm_neg_pos

# Create a score from this
climate$headline_sent_score <- as.numeric(dfm_neg_pos[,"positive"]) - as.numeric(dfm_neg_pos[,"negative"])
summary(climate$headline_sent_score)

#What are the 10 most positive?
positive_headlines <- climate[order(climate$headline_sent_score, decreasing= T), ]
head(positive_headlines$headline, 10)

#What are the 10 most negative?
negative_headlines <- climate[order(climate$headline_sent_score), ]
head(negative_headlines$headline, 10)