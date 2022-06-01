# State of the Union speeches
# Data Import
library(tidyverse)
library(tokenizers)

setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176/StateoftheUnionSimplified/StateoftheUnionSimplified")
# Metadata for the speeches
metadata <- read.csv("SOTU_WithText.csv")
metadata

# Words by year
words <- tokenize_words(metadata$text)

# create a new column for number of words
metadata$n_words <- NULL
for(i in 1:nrow(metadata)){
  metadata$n_words[i] <- length(words[[i]])
}

qplot(metadata$year, metadata$n_words)

# by the way speech was delivered
qplot(metadata$year, metadata$n_words,
      color = metadata$sotu_type)

