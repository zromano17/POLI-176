#State of the Union speeches.
#Data and code inspiration from http://programminghistorian.github.io/ph-submissions/lessons/published/basic-text-processing-in-r

#Install Necessary Packages (just need to do this once)
#install.packages("tidyverse")
#install.packages("tokenizers")
#install.packages("quanteda")
#Load package libraries
library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
#New library you will need:LDA and STM 
#install.packages("stm")
library(stm)
#install.packages("seededlda")
library(seededlda)
#Set working directory.  To find your working directory go to Session --> Set Working Directory --> Choose Directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176")
#Load data for speeches
metadata <- read_csv("SOTU_WithText.csv")
#Look at data
metadata

#Create a corpus of the state of the union speeches
corpus_sotu <- corpus(metadata, text_field = "text")
corpus_sotu

#Some common pre-processing
toks <- tokens(corpus_sotu, remove_punct = TRUE, remove_numbers=TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfm <- dfm(toks)

#We can trim the dfm of rare words
dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.05, docfreq_type = "prop")
dfm_trimmed

######
#LDA
######
#Run LDA using quanteda
lda <- textmodel_lda(dfm, k = 10)

#Most likely term for each topic
lda.terms <- terms(lda, 10)
lda.terms

#Topical content matrix
mu <- lda$phi
dim(mu) #10 topics, 5923 words
mu[1:10,1:20]
#Most representative words in Topic 1
mu[1,][order(mu[1,], decreasing=T)][1:10]

#Topical prevalence matrix
pi <- lda$theta
dim(pi) #number of docs by number of topics

#Most representative documents in Topic 1
metadata[order(pi[1,],decreasing=T),]

#STM
#Process the data to put it in STM format.  Textprocessor automatically does preprocessing
temp<-textProcessor(documents=metadata$text,metadata=metadata)
#prepDocuments removes words/docs that are now empty after preprocessing
out <- prepDocuments(temp$documents, temp$vocab, temp$meta)

#Let's try to distinguish between topics that are spoken/written and by year

#This takes a bit. You'd want to remove max.em.its -- this is just to make it shorter!
#Here we are using prevalence covariate sotu_type and year
model.stm <- stm(out$documents, out$vocab, K = 10, prevalence = ~sotu_type + s(year),
                 data = out$meta, max.em.its = 10) 

#Find most probable words in each topic
labelTopics(model.stm)
#And most common topics
plot(model.stm, n=10)

#Get representative documents
findThoughts(model.stm, texts=out$meta$text, topics=8, n=3)
findThoughts(model.stm, texts=out$meta$president, topics=8, n=10)
findThoughts(model.stm, texts=out$meta$year, topics=8, n=10)


#Estimate relationship between applause, order, and topics
model.stm.ee <- estimateEffect(1:10 ~ sotu_type + s(year), model.stm, meta = out$meta)
plot(model.stm.ee, "sotu_type")
plot(model.stm.ee, "sotu_type", method="difference", cov.value1="speech", cov.value2="written")
plot(model.stm.ee, "year", method="continuous", topics=8)


############################
#Optional: Using a content covariate
############################

#Let's just look at speeches made by Democratic and Republican presidents after 1950.
meta_sub <- metadata[metadata$party%in%c("Democratic", "Republican") & metadata$year>1950,]
#Process the data to put it in STM format.  Textprocessor automatically does preprocessing
temp<-textProcessor(documents=meta_sub$text,metadata=meta_sub)
#prepDocuments removes words/docs that are now empty after preprocessing
out <- prepDocuments(temp$documents, temp$vocab, temp$meta)

model.stm.c <- stm(out$documents, out$vocab, K = 10, content = ~party,
                 data = out$meta, max.em.its = 10) 
sageLabels(model.stm.c)

plot(model.stm.c, type="perspectives", topics=1)

findThoughts(model.stm.c, texts=out$meta$speech, topics=2, n=5)
plot(model.stm.c, type="perspectives", topics=2)
