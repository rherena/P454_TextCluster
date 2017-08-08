# Install Packages Necessary for the Analysis
Needed <- c("devtools", "tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph",
            "fpc", "topicmodels", "LDAvis", "syzuhet", "magritr", "stringi","textshape","stansent", "textreg")
if (length(setdiff(Needed, rownames(installed.packages()))) > 0) { install.packages((setdiff(Needed, rownames(installed.packages()))), dependencies=TRUE)
}
## Rcampdf is recommended by tm package but not available in CRAN
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
## Depending on your version of R, you might need to install from GitHub
# devtools::install_github(c("mjockers/syuzhet","trinker/textshape","trinker/stansent"))
library(magrittr) library(stringi)
setwd("C:/Users/Michael/Google Drive/MSPA Classes/PREDICT 453") # current working directory
# 1. Create a folder named "Texts" where you'll keep your data. # 2. Save the DSIs in the folder as .txt files
cname <- file.path("Texts") # Update to the appropriate path - use full path if not in current working directory
dir(cname) # shows files in the folder location
library(tm)
corp <- Corpus(DirSource(cname)) # Add files from the folder to the corpus
docs <- corp # Creating a copy of the corpus - might need the original later for processing sentiment
DSIList <- summary(docs) # stores a list of the files included in the corpus
##### Preprocessing Text #####
docs <- tm_map(docs, removeNumbers) # Removing numbers
docs <- tm_map(docs, content_transformer(tolower)) # Converting all text to lowercase
docs <- tm_map(docs, removeWords, stopwords("SMART")) #Removing stopwords - to see the list run stopwords("SMART")
# Removing all special characters - keeps alpha-numeric characters only removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
docs <- tm_map(docs, content_transformer(removeSpecialChars))
# docs <- tm_map(docs, removeWords, c("also","said")) # Code for removing additonal words (also and said are removed by SMART)

# Code for keeping words together/creating an equivalence class - add additional lines as necessary
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
## A more efficient way for creating equivalence classes but loses the DSI ID in the process # for (j in seq(docs))
#{
# docs[[j]] <- gsub("barak obama", "obama", docs[[j]])
# docs[[j]] <- gsub("hillary clinton", "clinton", docs[[j]])
# docs[[j]] <- gsub("ms clinton", "clinton", docs[[j]])
# docs[[j]] <- gsub("mrs clinton", "clinton", docs[[j]])
# docs[[j]] <- gsub("state department", "stdept", docs[[j]])
# docs[[j]] <- gsub("donald trump", "trump", docs[[j]])
# docs[[j]] <- gsub("mr sanders", "sanders", docs[[j]])
# docs[[j]] <- gsub("bernie sanders", "sanders", docs[[j]])
# docs[[j]] <- gsub("marco rubio", "rubio", docs[[j]])
# docs[[j]] <- gsub("ted cruz", "cruz", docs[[j]])
# docs[[j]] <- gsub("clintons", "clinton", docs[[j]])
# docs[[j]] <- gsub("emails", "email", docs[[j]]) #}
# library(SnowballC)
# docs <- tm_map(docs, stemDocument) # Stemming the Words in the Document - use with caution to avoid unexpected results
docs <- tm_map(docs, stripWhitespace) # Getting rid of extra whitespace
SENTIMENT ANALYSIS – HERO REPORT 18
# docs <- tm_map(docs, content_transformer(PlainTextDocument)) # This tells R to treat your preprocessed documents as text documents
##### Setting up data for analysis #####
dtm <- DocumentTermMatrix(docs, control = list(wordLengths=c(4,50))) # Creates the document term matrix
dtm
inspect(dtm[1:5, 1:20]) # taking a look at the first 20 terms in the first 5 DSIs
tdm <- TermDocumentMatrix(docs) # Creates the term document matrix tdm
inspect(tdm[1:5, 1:20]) # taking a look at the first 5 terms in the first 20 DSIs
# Common and Uncommon Words 
freq <- colSums(as.matrix(dtm)) 
length(freq)
ord <- order(freq)
freq[head(ord)] # Uncommon words freq[tail(ord)] # Common words
# # Eliminating Sparse Terms
dtms <- removeSparseTerms(dtm, 0.70) # This makes a matrix that is 70% empty space, maximum.
bagmatrix <- inspect((dtms))
write.csv((as.data.frame(bagmatrix, stringsAsFactors = FALSE)), file="Bag of Words DTM.csv") # exporting sparse matrix to .csv
freq <- sort(colSums(as.matrix(dtms)), decreasing=TRUE) # Switch to dtms when additional DSIs imported
freq
wf <- data.frame(word=names(freq), freq=freq) # Generating word frequencies head(wf) # Six most frequently occurring words
#Plotting terms
library(ggplot2)
ggplot(subset(wf, freq>40), aes(word, freq)) + geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + ggtitle("Frequenlty Occurring Terms (GT 40)")
#Creating a Word Cloud library(wordcloud)
SENTIMENT ANALYSIS – HERO REPORT 19
set.seed(142)
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) # Plotting high frequency terms
set.seed(142)
wordcloud(names(freq), freq, max.words=50, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) # Plotting the top 100 terms
##### Clustering #####
library(cluster)
termdist <- dist(scale(t(dtms)), method="euclidian") # Calculating distance between words
dsidist <- dist(scale(dtms), method="euclidian")
# Hierarchical Clustering
## Term Clusters
termfit <- hclust(d=termdist, method="ward.D") termfit
# Calculating distance between DSIs
# Clustering based on distance
plot(termfit, hang=-1) # Plotting the dendogram
groups <- cutree(termfit, k=7) # "k=" defines the number of clusters you are using rect.hclust(termfit, k=7, border="red") # draw dendogram with red borders around the 5 clusters
## DSI Clusters
dsifit <- hclust(d=dsidist, method="ward.D") # Clustering based on distance dsifit
plot(dsifit, hang=-1) # Plotting the dendogram
groups <- cutree(dsifit, k=4) # "k=" defines the number of clusters you are using rect.hclust(dsifit, k=4, border="red") # draw dendogram with red borders around the 5 clusters
# K-means Clustering
library(fpc)
## Term Clusters
# Looking for the "elbow" in order to select the number of term clusters wss <- 2:31
for (i in 2:31) wss[i] <- sum(kmeans(termdist,centers=i,nstart=25)$withinss)
plot(2:31, wss[2:31], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Term Clusters")
termkfit <- kmeans(termdist, 6) # Creating clusters
termkfit$cluster
clusplot(as.matrix(termdist), termkfit$cluster, color=T, shade=T, labels=2, lines=0)
## DSI Clusters
SENTIMENT ANALYSIS – HERO REPORT 20
# Looking for the "elbow" in order to select the number of DSI clusters
wss <- 2:31
for (i in 2:31) wss[i] <- sum(kmeans(dsidist,centers=i,nstart=25)$withinss)
plot(2:31, wss[2:31], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="DSI Clusters")
dsikfit <- kmeans(dsidist, 6) # Creating clusters
dsikfit$cluster
clusplot(as.matrix(dsidist), dsikfit$cluster, color=T, shade=T, labels=2, lines=0, main="Bag of Words DSI Clusters")
##### Latent Dirichlet Allocation ##### # Generating LDA Model library(topicmodels)
burnin <- 500
iter <- 1000 keep <- 30
k <- 6
LDAmodel <- LDA(dtm, k,
                method="Gibbs",
                control = list(burnin=burnin,
                               iter=iter, keep=keep))
ldaOut.topics <- as.matrix(topics(LDAmodel)) # DSIs by Topic ldaOut.topics
ldaOut.terms <- as.matrix(terms(LDAmodel,6)) # Top 6 terms by Topic ldaOut.terms
# Using model results to generate visualization
phi <- posterior(LDAmodel)$terms %>% as.matrix theta <- posterior(LDAmodel)$topics %>% as.matrix vocab <- colnames(phi)
doc.length <- vector()
for (i in 1:length(docs)) {
  temp <- paste(docs[[i]]$content, collapse = ' ')
  doc.length <- c(doc.length, stri_count(temp, regex = '\\S+')) }
temp_frequency <- inspect(dtm)
term.frequency <- data.frame(ST = colnames(temp_frequency),
                             Freq = colSums(temp_frequency)) rm(temp_frequency)
library(LDAvis)
SENTIMENT ANALYSIS – HERO REPORT 21
zson <- createJSON(K=K,
                   phi = phi, # terms.topics probs
                   theta = theta, # docs.topics probs
                   doc.length = doc.length, # vector with token counts for each document term.frequency = term.frequency$Freq, # vector of observed term frequencies vocab = vocab # vector containing the terms
)
serVis(zson) # LDA Results Visualization - can be viewed in browser for easier viewing
##### Beginning to Look at Sentiment #####
# Getting sentences out of corpus
# Break the corpora down into sentences and define some functions to do so. library(openNLP)
ToSentences = function(text, language="en") {
  # Splits text into sentences using an Apache OpenNLP sentence detector.
  # Arguments:
  # "text" the text to be processed (character)
  # "lang" ISO-639 code of the language of the text (character)
  # Returns:
  # sentences of the text (character vector)
  if(length(text) ==0) {return("")}
  if(nchar(text) == 0) {return("")} # Cover special case 0-character text.
  # Convert text to String object; allows for splitting by index. text = as.String(text)
  # Discover the sentence markers in the text (specify NLP as
  # source of annotate because there is also an annotate function in ggplot2) markers = NLP::annotate(
  text,
  Maxent_Sent_Token_Annotator(language=language) # Annotator from OpenNLP )
  # Return sentences by splitting the text at the boundaries.
  text[markers] }
# Convert the corpus to a character vector
text = lapply(docs, "[[", "content") # Extracting text from Corpus
text = lapply(text, ToSentences) # Converting text to sentences (in this case converting the entire DSI to 1 string)
text = as.vector(unlist(text)) # Converting the DSIs to a character vector
head(text)
SENTIMENT ANALYSIS – HERO REPORT 22
# Use the character vector for sentiment analysis library(syuzhet)
syuzhet_vector <- get_sentiment(text, method="syuzhet") bing_vector <- get_sentiment(text, method="bing") afinn_vector <- get_sentiment(text, method="afinn") nrc_vector <- get_sentiment(text, method="nrc")
library(stansent) # Stanford Sentiment (available in syuzhet but does not currenlty work on Windows based PC)
stanford_vector <- sentiment_stanford(text, hyphen = "", missing_value = 0,
                                      stanford.tagger = "F:/stanford-corenlp-full-2015-12-09", java.path =
                                        "java") stanford_vector
DSISentSigns<-as.data.frame(cbind(DSIList[,-c(1:3)], # Combining list of DSIs and various sentiment signs
                                  syuzhet=sign(syuzhet_vector), bing=sign(bing_vector), afinn=sign(afinn_vector), nrc=sign(nrc_vector), stanford=sign(stanford_vector$sentiment)
)) DSISentSigns
nrc_data <- get_nrc_sentiment(text) # Separate run of nrc sentiment to get full output nrc_data[, 1:8] # nrc output of emotions
DSISentiment<- as.data.frame(cbind(DSIList[,-c(1:3)],syuzhet_vector, bing_vector, afinn_vector,nrc_vector, stanford=stanford_vector$sentiment)) #
Combining list of DSIs and various sentiment scores
indx <- sapply(DSISentiment, is.factor)
DSISentiment[indx] <- lapply(DSISentiment[indx], function(x) as.numeric(as.character(x))) # converting scores to numeric
DSISentiment
#### Cohort 4 TDM (as developed/refined by Jillian and Rhonda) ####
Cohort4 <- read.csv("C:/Users/Michael/Google Drive/MSPA Classes/PREDICT 453/453_C 1_4_Hero ProjectTDM.csv",header=T, row.names=1) # load the Cohort TDM.csv" file Cohort4 <- as.TermDocumentMatrix(Cohort4, weightTf) # Specifinging as a term document matrix
# # Eliminating Sparse Terms
Cohort4s <- removeSparseTerms(Cohort4, 0.70) # This makes a matrix that is 70% empty space, maximum.
SENTIMENT ANALYSIS – HERO REPORT 23
C4matrix <- inspect((Cohort4s))
write.csv((as.data.frame(t(C4matrix), stringsAsFactors = FALSE)), file="Cohort 4 DTM.csv") # exporting sparse matrix to .csv
C4freq <- sort(colSums(as.matrix(t(Cohort4s))), decreasing=TRUE) C4freq
C4wf <- data.frame(word=names(C4freq), freq=C4freq) # Generating word frequencies head(C4wf) # Six most frequently occurring words
## Clustering on TDM##
C4dsidist <- dist(scale(t(Cohort4s)), method="euclidian") # Calculating distance between DSIs
# Hierarchical Clustering
## DSI Clusters
C4dsifit <- hclust(d=C4dsidist, method="ward.D") # Clustering based on distance C4dsifit
plot(C4dsifit, hang=-1) # Plotting the dendogram
C4groups <- cutree(C4dsifit, k=5) # "k=" defines the number of clusters you are using rect.hclust(C4dsifit, k=5, border="red") # draw dendogram with red borders around the 5 clusters
# K-means Clustering
# Looking for the "elbow" in order to select the number of DSI clusters
wss <- 2:31
for (i in 2:31) wss[i] <- sum(kmeans(C4dsidist,centers=i,nstart=25)$withinss)
plot(2:31, wss[2:31], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="DSI Clusters")
## DSI Clusters
C4dsikfit <- kmeans(C4dsidist, 6)
C4Cluster <- C4dsikfit$cluster
C4Cluster
clusplot(as.matrix(C4dsidist), C4dsikfit$cluster, color=T, shade=T, labels=2, lines=0, main="Cohort 4 Terms DSI Clusters")
## Cluster Sentiment
C4sentiment <- as.data.frame(cbind(DSISentiment,C4Cluster))
indx <- sapply(C4sentiment, is.factor)
C4sentiment[indx] <- lapply(C4sentiment[indx], function(x) as.numeric(as.character(x))) head(C4sentiment)
C4Sentiment <-aggregate(C4sentiment[,c(1:5)], by=list(C4Cluster),FUN=sum, na.rm=TRUE) C4Sentiment