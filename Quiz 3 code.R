library(tidyverse)
library(quanteda)

#Set working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176/Discussion3-1/Discussion3")
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

############ Mutal Information ###############

#Mutual Information function
mi <- function(dfm, clust.vect){
  np <- sum(clust.vect)
  ns <- sum(!clust.vect)
  D = np + ns
  nj <- apply(dfm,2,function (x) sum(x>0))
  nnotj <- apply(dfm,2,function (x) sum(x==0))
  njp <- apply(dfm[clust.vect,], 2, function (x) sum(x>0))
  njs <- apply(dfm[!clust.vect,], 2, function (x) sum(x>0))
  nnotjp <- apply(dfm[clust.vect,], 2, function (x) sum(x==0))
  nnotjs <- apply(dfm[!clust.vect,], 2, function (x) sum(x==0))
  mi <- njp/D*log((njp*D)/(np*nj),2)+ njs/D*log((njs*D)/(nj*ns),2) +
    nnotjp/D*log((nnotjp*D)/(np*nnotj),2) +
    nnotjs/D*log((nnotjs*D)/(nnotj*ns),2) 
  names(mi) <- colnames(dfm)
  return(mi)
}

#np: Total number of spoken speeches
np <- sum(metadata$sotu_type=="speech")
#ns: Total number of written speeches
ns <- sum(metadata$sotu_type=="written")
#Number of spoken speeches that contain word j
njp <- apply(dfm_trimmed[metadata$sotu_type=="speech",], 2, function (x) sum(x>0))
#Number of written speeches that contain word j
njs <- apply(dfm_trimmed[metadata$sotu_type=="written",], 2, function (x) sum(x>0))


#Calculate mutual information
mi_speech <- mi(dfm_trimmed, metadata$sotu_type=="speech")

#Find 5 words most indicative of spoken speeches using mutual information
speechwords <- sort(mi_speech[njp/np-njs/ns>0], decreasing=T)[1:5]
speechwords

############ Fightin' Words ###############

# Fightin' Words Function
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

#Find 5 words most indicative of spoken speeches using fightin' words
speech_terms <- clusterFightinWords(dfm_trimmed, 
                             metadata$sotu_type == "speech")
sort(speech_terms, decreasing=T)[1:5]
