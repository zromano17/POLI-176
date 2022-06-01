#State of the Union speeches.
#Data and code inspiration from http://programminghistorian.github.io/ph-submissions/lessons/published/basic-text-processing-in-r

#Install Necessary Packages (just need to do this once)
#install.packages("tidyverse")
#install.packages("tokenizers")
install.packages("quanteda")
#Load package libraries

library(tidyverse)
library(tokenizers)
library(quanteda)

#Set working directory.  To find your working directory go to Session --> Set Working Directory --> Choose Directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176/Discussion2-1/Discussion2")

#Load data for speeches
metadata <- read_csv("SOTU_WithText.csv")
#Look at data
metadata

#Create a corpus of the state of the union speeches
corpus_sotu <- corpus(metadata, text_field = "text")
corpus_sotu

#Create a document feature matrix (dfm)
dfm <- dfm(corpus_sotu)
dfm
#32,760 features, wow!
#Let's look at that in a word cloud
textplot_wordcloud(dfm, col="black")

#Some common pre-processing
dfm <- dfm(corpus_sotu,remove_punct=TRUE, remove=c(stopwords("en"), "$"), stem=T)
dfm
#21,390 -- a bit better
#Let's look at that in a word cloud
textplot_wordcloud(dfm, col="black")

#More options in dfm
?dfm

#We can trim the dfm of rare words
dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.05, docfreq_type = "prop")
dfm_trimmed
textplot_wordcloud(dfm_trimmed, col="black")

#Let's subset our document term matix to just written speeches and plot
textplot_wordcloud(dfm_trimmed[docvars(dfm_trimmed, "sotu_type")=="written",])
textplot_wordcloud(dfm_trimmed[docvars(dfm_trimmed, "sotu_type")=="speech",])

#ADVANCED (optional): Mosteller and Wallace Example
#Data for example from http://ptrckprry.com/course/ssd/lecture/federalist.html

library(jsonlite)
#Read in json file
fed <- stream_in(file("federalist.json"))
#Take a look at it
names(fed)
#standardize author
fed$author <- tolower(fed$author)
table(fed$author)

#Import as a corpus
corpus_raw <- corpus(fed, text_field = "text")
#Preprocess
dfm_papers <- dfm(corpus_raw, remove_numbers = TRUE, tolower = TRUE,
                remove_punct = TRUE, verbose = TRUE)
dfm_papers

#Next step calculate similarity between unknown paper 49 and hamilton and madison corpuses
#Cosine Similarity function
cosine_sim <- function(a, b) crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))

#Create three vectors -- one for hamilton, madison and unknown by summing over the docs
hamilton <- apply(dfm_papers[docvars(dfm_papers, "author")=="hamilton",], 2, sum)
madison <- apply(dfm_papers[docvars(dfm_papers, "author")%in%c("madison"),], 2, sum)
unknown <- apply(dfm_papers[docvars(dfm_papers, "paper_id")==49,], 2, sum)

#Similarity between hamilton and unknown
cosine_sim(hamilton, unknown)
#Similarity between madison and unknown
cosine_sim(madison, unknown)

#Create a tf-idf document term matrix
# w * log(N/n) N=n
dfm_papers_tfidf <- dfm_tfidf(dfm_papers)
dfm_papers_tfidf[1,]
dfm_papers[1,]

#Create three vectors -- one for hamilton, madison and unknown by summing over the docs
hamilton <- apply(dfm_papers_tfidf[docvars(dfm_papers_tfidf, "author")%in%c("hamilton"),], 2, sum)
madison <- apply(dfm_papers_tfidf[docvars(dfm_papers_tfidf, "author")%in%c("madison"),], 2, sum)
unknown <- apply(dfm_papers_tfidf[docvars(dfm_papers_tfidf, "paper_id")==49,], 2, sum)

#Similarity between hamilton and unknown
cosine_sim(hamilton, unknown)
#Similarity between madison and unknown
cosine_sim(madison, unknown)
