library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
setwd("~/Documents/Text Analysis Pred 453/P454_TextCluster/")


cname <- file.path("DSI") 
dir(cname) 
corp <- Corpus(DirSource(cname)) 
docs <- corp 
DSIList <- summary(docs)


docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, content_transformer(tolower)) 
docs <- tm_map(docs, removeWords, stopwords("SMART")) 
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
docs <- tm_map(docs, content_transformer(removeSpecialChars))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

# Equivalence Classes, Initial from last cohort, we can add more as we make more passes on DTM
docs <- tm_map(docs, content_transformer(gsub), pattern = "barak obama", replacement = "obama")
docs <- tm_map(docs, content_transformer(gsub), pattern = "hillary clinton", replacement = "hclinton")
docs <- tm_map(docs, content_transformer(gsub), pattern = "ms clinton", replacement = "hclinton")
docs <- tm_map(docs, content_transformer(gsub), pattern = "mrs clinton", replacement = "hclinton")
docs <- tm_map(docs, content_transformer(gsub), pattern = "state department", replacement = "stdept")
docs <- tm_map(docs, content_transformer(gsub), pattern = "donald trump", replacement = "dtrump")
docs <- tm_map(docs, content_transformer(gsub), pattern = "mr sanders", replacement = "bsanders")
docs <- tm_map(docs, content_transformer(gsub), pattern = "bernie sanders", replacement = "bsanders")
docs <- tm_map(docs, content_transformer(gsub), pattern = "marco rubio", replacement = "mrubio")
docs <- tm_map(docs, content_transformer(gsub), pattern = "ted cruz", replacement = "tcruz") 
docs <- tm_map(docs, content_transformer(gsub), pattern = "emails", replacement = "email")

dtm <- DocumentTermMatrix(docs, control = list(wordLengths=c(4,50)))
inspect(dtm)
tdm <- TermDocumentMatrix(docs) 
inspect(tdm)


# Write Matrix and o
dtms <- removeSparseTerms(dtm, .75) # This makes a matrix that is 70% empty space, maximum.
inspect(dtms)
dtms <- t(as.matrix(dtms))
write.csv(dtms, "dtms.csv")
