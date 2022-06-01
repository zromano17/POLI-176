# Load package libraries
library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(stm)
library(seededlda)

# Set working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176")

############## 2.1 #################

# Load data
data <- read.csv("UNGDspeeches.csv")

# Use common bag of words pre-processing to create a document feature matrix

# Create a corpus of the speech data
corpus <- corpus(data, text_field = "text")

# Remove punctuation and numbers
toks <- tokens(corpus, remove_punct = TRUE, remove_numbers=TRUE)

# Stem the tokens
toks <- tokens_wordstem(toks)

# Remove stop words
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")

# Create a document feature matrix
dfm <- dfm(toks)

# Trim the dfm to include only word that appear in at least 5% of the documents
dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.05, docfreq_type = "prop")

# Check number of features in dfm_trimmed
dfm_trimmed

############## 2.2 #################

# Plot a word cloud of dfm_trimmed
textplot_wordcloud(dfm_trimmed)

############## 3.1 #################

# Subset both the dfm and the metadata only to speeches made by the US and Canada
dfm_usa_can <- dfm_trimmed[docvars(dfm_trimmed, "country") == "USA" | docvars(dfm_trimmed, "country") == "CAN", ]

data_usa_can <- data[data$country == "USA" | data$country == "CAN", ]

############## 3.2 #################

# Use Fightin' Words to find the top 10 words that most distinguish speeches made by Canada from those made by the US

# Fightin' Words function
clusterFightinWords <- function(dfm, clust.vect, alpha.0=100) {
  overall.terms <- colSums(dfm)
  n <- sum(overall.terms)
  prior.terms <- overall.terms / n * alpha.0
  cluster.terms <- colSums(dfm[clust.vect, ])
  cluster.n <- sum(cluster.terms)
  
  cluster.term.odds <- 
    (cluster.terms + prior.terms) / 
    (cluster.n + alpha.0 - cluster.terms - prior.terms)
  overall.term.odds <- 
    (overall.terms + prior.terms) / 
    (n + alpha.0 - overall.terms - prior.terms)
  
  log.odds <- log(cluster.term.odds) - log(overall.term.odds)
  
  variance <- 1/(cluster.terms + prior.terms) + 1/(overall.terms + prior.terms)
  
  # return the variance weighted log-odds for each term
  output <- log.odds / sqrt(variance)
  names(output) <- colnames(dfm)
  return(output)
}

# Use Fightin' Words to find the top 10 words that most distinguish speeches made by Canada from those made by the US
terms_can <- clusterFightinWords(dfm_usa_can, 
                                 data_usa_can$country == "CAN")
sort(terms_can, decreasing=T)[1:10]

############## 3.3 #################

# Use Fightin' Words to find the top 10 words that most distinguish speeches made by the US from those made by Canada
terms_usa <- clusterFightinWords(dfm_usa_can, 
                                 data_usa_can$country == "USA")
sort(terms_usa, decreasing=T)[1:10]

############## 4.1 #################

# Preprocess the subsetted data using textProcessor and prepDocuments in STM format.
data2 <- textProcessor(documents = data_usa_can$text, metadata = data_usa_can)
data_stm <- prepDocuments(data2$documents, data2$vocab, data2$meta)

############## 4.2 #################

# Run a STM using the stm function with K = 10 and  prevalence = ~ country + s(year)
model.stm <- stm(data_stm$documents, data_stm$vocab, K = 10, prevalence = ~country + s(year),
                 data = data_stm$meta, max.em.its = 10) 

# Use the labelTopics function to look at the 10 topics
labelTopics(model.stm)

############## 4.3 #################

# Use the labelTopics and findThoughts functions to label each of the 10 topics
labelTopics(model.stm)

findThoughts(model.stm, texts = data_stm$meta$text, topics = 1, n = 3)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 2, n = 3)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 3, n = 3)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 4, n = 3)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 5, n = 3)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 6, n = 3)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 7, n = 3)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 8, n = 3)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 9, n = 3)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 10, n = 3)
 
############## 4.4 #################

# Use the estimateEffect function to estimate the relationship between the topics and country and year
model.stm.ee <- estimateEffect(1:10 ~country + s(year), model.stm, meta = data_stm$meta)

# Using the method "continuous", plot one of the topics over year
par(mfrow=c(1,1))
plot(model.stm.ee, "year", method="continuous", topics = 1, legend('topleft'), main = "Topic 1")
plot(model.stm.ee, "year", method="continuous", topics = 2, legend('topleft'))
plot(model.stm.ee, "year", method="continuous", topics = 3, legend('topleft'))
plot(model.stm.ee, "year", method="continuous", topics = 4, legend('topleft'))
