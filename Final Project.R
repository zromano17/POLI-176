# Zachary Romano
# POLI 176
# Final Project
# May 28, 2021
################################################################################

# Clear Workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(stm)

# Set working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176")

# Load data
data <- read_csv("USCongress.csv")

# Preprocess the data using textProcessor and prepDocuments in STM format.
data2 <- textProcessor(documents = data$text, metadata = data)
data_stm <- prepDocuments(data2$documents, data2$vocab, data2$meta)

# Distinguish between topics that are introduced in the House and the Senate
model.stm <- stm(data_stm$documents, data_stm$vocab, K = 10, prevalence = ~h_or_sen,
                 data = data_stm$meta)

# Find most probable words in each topic
labelTopics(model.stm)

# Look at some of the bill summaries within each topic
# Use this to help create labels for each topic
findThoughts(model.stm, texts = data_stm$meta$text, topics = 1, n = 20)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 2, n = 20)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 3, n = 20)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 4, n = 20)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 5, n = 20)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 6, n = 20)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 7, n = 20)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 8, n = 20)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 9, n = 20)
findThoughts(model.stm, texts = data_stm$meta$text, topics = 10, n = 20)

# Look at distribution of 10 topics
plot(model.stm, n = 10)

# Estimate the difference in topic prevalence between House bills and Senate bills
model.stm.ee <- estimateEffect(1:10 ~ h_or_sen, model.stm, meta = data_stm$meta)

# Plot the difference in topic prevalence
plot(model.stm.ee, "h_or_sen", method= "difference", cov.value1="HR", cov.value2="S")