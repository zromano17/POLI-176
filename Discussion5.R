##Discussion 5
#News Snippets about Climate Change from: https://blog.gdeltproject.org/a-new-contextual-dataset-for-exploring-climate-change-narratives-6-3m-english-news-urls-with-contextual-snippets-2015-2020/

#Let's look at those for 2020
library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(devtools)
devtools::install_github("quanteda/quanteda.corpora")
library(quanteda.corpora)
library(irr)

#Read the data into R
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176")
climate <- read_csv("WebNewsEnglishSnippets.2020.csv", col_names=F)
names(climate) <- c("date", "headline", "image", "url", "snippet")
climate

#Let's pretend we are interested in using the snippet for TAD 
corpus_climate <- corpus(climate, text_field = "headline")
corpus_climate

#Create a document feature matrix (dfm)
#Some common pre-processing
toks <- tokens(corpus_climate, remove_punct = TRUE, remove_numbers=TRUE)
#We are going to use a dictionary to start, so I won't stem (commenting stemming out)
#toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfm <- dfm(toks)

#We'll use the lexicoder sentiment dictionary that's in quanteda
head(data_dictionary_LSD2015)

#Select only the "negative" and "positive" categories
data_dictionary_LSD2015_pos_neg <- data_dictionary_LSD2015[1:2]

#Categorize each word into positive or negative and count them per document
toks_gov_dict <- tokens_lookup(toks, dictionary = data_dictionary_LSD2015_pos_neg)

#Create a dfm of the result
dfm_neg_pos <- dfm(toks_gov_dict)
dfm_neg_pos

#Create a score from this
climate$snippet_sent_score <- as.numeric(dfm_neg_pos[,"positive"]) - as.numeric(dfm_neg_pos[,"negative"])
summary(climate$snippet_sent_score)

#What are the most positive?
climate$snippet[climate$snippet_sent_score>9]

#What are the most negative?
climate$snippet[climate$snippet_sent_score< -10]

#Ok, let's try to do some hand coding instead.
#Let's define a category called ``urgency'' where we want to code 1 if the snippet 
#expresses a sense of urgency about climate change

#Define urgency

#First randomly select 200 snippets for hand coding
set.seed(92073)
handcoding <- climate[sample(1:nrow(climate),200),]
#write.csv(handcoding, "ForHandCoding.csv", row.names=F)
#I loaded this into Google Drive, let's hand code it!
#https://docs.google.com/spreadsheets/d/1Jn0w9Zc4xm4R_4AeDJoAE5cbGMuj4BaHpQZWTTkj4GQ/edit?usp=sharing
#Put in 1 if urgency, 0 if not urgency

#Let's look at intercoder reliability
handcoding <- read.csv()
#Confusion Matrix
table(handcoding$Urgency1, handcoding$Urgency2)
#Krippendorff's alpha
kripp.alpha(t(handcoding[,c("Urgency1", "Urgency2")]))
