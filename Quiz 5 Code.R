# Load libraries
library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textmodels)

setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176")
congress <- read.csv("USCongress.csv")

# Create two categories -- health and not health
congress$health <- ifelse(congress$label%in%("Health"), 1, 0)

# Preprocess the data
corpus_congress <- corpus(congress, text_field = "text")
corpus_congress

# Remove punctuation, numbers
toks <- tokens(corpus_congress, remove_punct = TRUE, remove_numbers=TRUE)
# Stem words
toks <- tokens_wordstem(toks)
# Remove stop words
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")

# Create a document feature matrix (dfm)
dfm <- dfm(toks)

# Split into training and validation
set.seed(92073)
docvars(corpus_congress, "id_numeric") <- 1:ndoc(corpus_congress)
alldocs <- 1:ndoc(corpus_congress)
training <- sample(alldocs, round(length(alldocs)*.75))
validation <- alldocs[!alldocs%in%training]

# Create separate dfm's for each
dfmat_train <- dfm_subset(dfm, docvars(corpus_congress, "id_numeric")%in%training)
dfmat_val <- dfm_subset(dfm, docvars(corpus_congress, "id_numeric")%in%validation)

# Naive Bayes
# Train
tmod_nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "health"))
summary(tmod_nb)

# Probability of a word given a category
coef_nb <- coef(tmod_nb)
head(coef_nb)

# 10 words most associated with health:
sort(coef_nb[,2]/coef_nb[,1], decreasing=T)[1:10]

#How well does this prediction do out of sample? Validation
predict.val <- predict(tmod_nb, newdata = dfmat_val)

tab_val <- table(docvars(dfmat_val, "health"), predict.val)
tab_val

# Precision
diag(tab_val)/colSums(tab_val)
# Recall
diag(tab_val)/rowSums(tab_val)
