#Climate Data, Supervised Learning
library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textmodels)

#Motivation for the example from Professor Soltoff's 
#Computing for the Social Sciences class at the University of Chicago
#https://cfss.uchicago.edu/notes/supervised-text-classification/

setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 176")
congress <- read_csv("USCongress.csv")

#Hand labeled bills from the U.S. congress. 
#Codebook: https://www.comparativeagendas.net/pages/master-codebook
congress

#Let's look at a table of labels
sort(table(congress$label))

#For simplicity, we are going to create two categories -- international category and domestic
congress$international <- ifelse(congress$label%in%c("International affairs and foreign aid ",
                                                     "Foreign trade", "Defense"),1,0)

#We will try to create a classifier for international/not international
#Preprocess the data
corpus_congress <- corpus(congress, text_field = "text")
corpus_congress

#Create a document feature matrix (dfm)
#Some common pre-processing
toks <- tokens(corpus_congress, remove_punct = TRUE, remove_numbers=TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfm <- dfm(toks)

#Split into training and validation
set.seed(92073)
docvars(corpus_congress, "id_numeric") <- 1:ndoc(corpus_congress)
alldocs <- 1:ndoc(corpus_congress)
training <- sample(alldocs, round(length(alldocs)*.75))
validation <- alldocs[!alldocs%in%training]

#Create separate dfm's for each
dfmat_train <- dfm_subset(dfm, docvars(corpus_congress, "id_numeric")%in%training)
dfmat_val <- dfm_subset(dfm, docvars(corpus_congress, "id_numeric")%in%validation)

#Naive Bayes
#Train
tmod_nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "international"))
summary(tmod_nb)

#Probability of a word given a category
coef_nb <- coef(tmod_nb)
head(coef_nb)

#Words associated with international:
sort(coef_nb[,2]/coef_nb[,1], decreasing=T)[1:20]
#Words not associated with international
sort(coef_nb[,2]/coef_nb[,1], decreasing=F)[1:20]

#How well does it do in sample?
predict.train <- predict(tmod_nb, dfmat_train)

tab_train <- table(docvars(dfmat_train, "international"), predict.train)
tab_train

#precision
diag(tab_train)/colSums(tab_train)
#recall
diag(tab_train)/rowSums(tab_train)

#How well does this prediction do out of sample?  Validation
predict.val <- predict(tmod_nb, newdata = dfmat_val)

tab_val <- table(docvars(dfmat_val, "international"), predict.val)
tab_val

#precision
diag(tab_val)/colSums(tab_val)
#recall
diag(tab_val)/rowSums(tab_val)


#Lasso 
library(glmnet)
lasso.1 <- glmnet(dfmat_train, docvars(dfmat_train, "international"),
                  family="binomial", alpha=1)

#These are all of the different lambdas glmnet used
lasso.1$lambda
#These lambdas produce different betas:
summary(lasso.1$beta[,1])
summary(lasso.1$beta[,40])
sort(lasso.1$beta[,40], decreasing=T)[1:40]
sort(lasso.1$beta[,40], decreasing=F)[1:40]

#Let's look at it's performance out of sample
predict.test <- predict(lasso.1, dfmat_val, type="class")
table(docvars(dfmat_val, "international"),as.numeric(predict.test[,40]))
table(docvars(dfmat_val, "international"),as.numeric(predict.test[,1]))

#####
#Cross Validation
#####

#Cross-validation with Lasso
cv <- cv.glmnet(dfmat_train, docvars(dfmat_train, "international"),
                family="binomial", alpha=1, 
                type.measure="mse")
plot(log(cv$lambda), cv$cvm, xlab="Log Lambda", ylab="Mean Cross-Validated Error")

#Predict for the test set
predict.test <- predict(cv, dfmat_val, type="class")

#How many bills are international?
prop.table(table(predict.test))
#What is the truth?
prop.table(table(docvars(dfmat_val, "international")))

#Let's look at some of the predicted
test_texts <- as.character(corpus_congress)[validation]
sample(test_texts[predict.test=="1"],10)
sample(test_texts[predict.test=="0"],10)

#Predictability as Measurement
predict.test <- predict(cv, dfm)
congress$international_score <- predict.test
par(mfrow=c(1,2))
hist(congress$international_score[congress$h_or_sen=="S"], main="International, Senate", xlim=c(-7,10))
hist(congress$international_score[congress$h_or_sen=="HR"], main="International, House", xlim=c(-7,10))

