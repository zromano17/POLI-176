## Discriminating Words
#Mosteller and Wallace Example
#Data for example from http://ptrckprry.com/course/ssd/lecture/federalist.html

library(jsonlite)
library(quanteda)
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176/Discussion3-1/Discussion3")
#Read in json file
fed <- stream_in(file("federalist.json"))
#Take a look at it
names(fed)
#Standardize author
fed$author <- tolower(fed$author)
table(fed$author)

#Import as a corpus
corpus_raw <- corpus(fed, text_field = "text")
#Preprocess
toks <- tokens(corpus_raw, remove_punct = TRUE, remove_numbers=TRUE)
toks <- tokens_wordstem(toks)
dfm <- dfm(toks)

#Trim corpus to only include words that appear in more than 20% of documents
dfm_papers <- dfm_trim(dfm, min_docfreq = 0.2, docfreq_type = "prop")
dfm_papers

#Subset dfm and meatadata to only paper written by hamilton and madison
dfm_papers <- dfm_papers[fed$author%in%c("hamilton", "madison"),]
fed <- fed[fed$author%in%c("hamilton", "madison"),]

#Which words distinguish hamilton and madison
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

#calculate mutual information
mi_hamilton <- mi(dfm_papers, fed$author=="hamilton")

#Calculate difference in proportion for x-axis
#np: Total number of hamilton documents
np <- sum(fed$author=="hamilton")
#ns: Total number of madison documents
ns <- sum(fed$author=="madison")
#Number of hamilton documents that contain word j
njp <- apply(dfm_papers[fed$author=="hamilton",], 2, function (x) sum(x>0))
#Number of madison documents that contain word j
njs <- apply(dfm_papers[fed$author=="madison",], 2, function (x) sum(x>0))

#Plot mutual information
plot(njp/np-njs/ns, mi_hamilton, col="white", 
     ylab="Mutual Information",
     xlab="Madison <--> Hamilton", main="",
     cex.axis=1.2, cex.lab=1.5)
text(njp/np-njs/ns, mi_hamilton, names(mi_hamilton), cex=mi_hamilton/max(mi_hamilton, na.rm=T)+.3)

#We found the pattern!
#https://www.scientificamerican.com/article/how-a-computer-program-helped-show-jk-rowling-write-a-cuckoos-calling/
hamiltonwords <- sort(mi_hamilton[njp/np-njs/ns>0], decreasing=T)[1:20]
madisonwords <- sort(mi_hamilton[njp/np-njs/ns<0], decreasing=T)[1:20]
hamiltonwords
madisonwords


#A different perspective: Fightin' words
clusterFightinWords <- function(dfm, clust.vect, alpha.0=100) {
  # we need to get the overall corpus word distribution and the cluster-specific words dists
  # y_{kw} in Monroe et al. 
  overall.terms <- colSums(dfm)
  # n and n_k in Monroe et al. 
  n <- sum(overall.terms)
  # alpha_{kw} in Monroe et al. 
  prior.terms <- overall.terms / n * alpha.0
  # y_{kw}(i) in Monroe et al.
  cluster.terms <- colSums(dfm[clust.vect, ])
  # n_k(i) in Monroe et al.
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

terms <- clusterFightinWords(dfm_papers, 
                             fed$author=="madison")
sort(terms, decreasing=T)[1:20]

terms <- clusterFightinWords(dfm_papers, 
                             fed$author=="hamilton")
sort(terms, decreasing=T)[1:20]

library(tidyverse)
#For State of the Union
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

#Find words that are distinctive between speeches written by Republicans versus the rest of the corpus
terms <- clusterFightinWords(dfm_trimmed, 
                             metadata$sotu_type=="speech")
sort(terms, decreasing=T)[1:5]


