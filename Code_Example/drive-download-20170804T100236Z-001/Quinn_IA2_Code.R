# Quinn text visualization assignment

# Load packages
library(tm)
library(SnowballC)
library(som)
library(tau)
library(wordcloud)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggthemes)

setwd("~/Documents/MSPA/455 - Data Viz/IA2/Quinn_IA2")
#setwd("~/MSPA/455 - Data Viz/IA2/POTUS_Candidate_Twitter_Data")

# Read in the NRC Emotion/Sentiment Lexicon
emolex <- read.table("NRC_emotion_lexicon.txt")
names(emolex) <- c("word", "quality", "yes")

#Function to find the top ten words
Top10Words = function(df){
  sort(colSums(df), decreasing=TRUE)[c(1:10)]
}


###### BERNIE SANDERS ########

# Read in the data
bs <- read.csv("berniesanders_tweets.csv", stringsAsFactors = FALSE)

# Create stop words, including campagin slogan
bs_stopwords <- c('Bernie', 'Sanders', 'berniesanders', 'bernie', 'sanders', 'feelthebern')

# Pre-process the corpus to remove non-alphanumeric characters, links, numbers
# Remove punctuation, change to lower case, remove stopwords
bs_corpus <- Corpus(VectorSource(bs$tweet_text))
for(j in seq(bs_corpus))   
{   
  bs_corpus[[j]] <- gsub("[^[:alnum:] ]", "", bs_corpus[[j]])
  bs_corpus[[j]] <- gsub("http.*", "", bs_corpus[[j]])
}
bs_corpus <- tm_map(bs_corpus, removeNumbers) 
bs_corpus <- tm_map(bs_corpus, tolower)
bs_corpus <- tm_map(bs_corpus, removePunctuation)
bs_corpus <- tm_map(bs_corpus, removeWords, c(stopwords("english"),bs_stopwords))
bs_corpus <- tm_map(bs_corpus, PlainTextDocument)

# Create a Document Term Matrix and convert it to a dataframe
bs_frequencies <- DocumentTermMatrix(bs_corpus)
bs_df <- as.data.frame(as.matrix(bs_frequencies))
#save document term matrix dataframe
write.csv(bs_df, "sanderstweets_dtm.csv")

# Remove sparse terms -- terms that no appear in at least 0.5% of tweets
bs_sparse <- removeSparseTerms(bs_frequencies, 0.995)
bs_sparse <- as.data.frame(as.matrix(bs_sparse))
colnames(bs_sparse) = make.names(colnames(bs_sparse))

# Sort the sparsed dataframe by decreasing frequency and set the color levels for the word cloud
bs_wordFreqsort <- sort(colSums(bs_sparse),decreasing=T)
bs_grayLevels <- gray((bs_wordFreqsort + 10)/(max(bs_wordFreqsort)+10))

pdf(file = "Sanders_wordcloud.pdf", 
    width = 7.5, height = 7.5, paper = "letter")  
bs_wordCloud <- wordcloud(words=names(bs_wordFreqsort),freq=bs_wordFreqsort,min.freq=50,random.order=T,colors=bs_grayLevels)
dev.off()

# Cluster analysis
bs2 <- bs_sparse
bs2[bs2>1]=1

bsNorm <- bs2
bsNorm_Dist <- dist(bsNorm, method="manhattan") # compute distances
bsNorm_HC <- hclust(bsNorm_Dist, method="ward.D") # make model
plot(bsNorm_HC) # plot the dendogram
bsNorm_HC_Groups <- cutree(bsNorm_HC, k = 6) # split data into clusters

#Visualize the clusters
lapply(split(bs_sparse, bsNorm_HC_Groups), Top10Words)

#Create dataframe with word counts
bs_colsums <- colSums(bs_sparse)
bs_df_final <- cbind(colnames(bs_sparse), bs_colsums)
bs_df_final <- as.data.frame(bs_df_final)
rownames(bs_df_final) <- c()
colnames(bs_df_final) <- c('word', 'count')

bs_df_final <- merge(bs_df_final, emolex, by.x = "word", by.y = "word")
bs_df_final$count <- as.numeric(bs_df_final$count)
bs_df_final$total <- (bs_df_final$count)*(bs_df_final$yes)
bs3 <- filter(bs_df_final, yes == 1)
bs3 <- filter(bs3, quality != "positive")
bs3 <- filter(bs3, quality != "negative")
bs4 <- filter(bs_df_final, yes == 1)
bs4 <- filter(bs4, quality %in% c("positive", "negative"))

#Get total count for charts below
sum(bs3$total)
sum(bs4$total)

#Visualize the snetiments and emotions
pdf(file = "Sanders_emotions.pdf", 
    width = 7.5, height = 7.5, paper = "letter")  
ggplot_object <- ggplot(bs3, aes(x = quality, y = (total/4351))) + geom_bar(stat = "identity") + theme_fivethirtyeight() + ggtitle("Bernie Sanders tweets") + scale_y_continuous(breaks = seq(0,.3,.1), labels = c("", "10%", "20%", "30%"), limits = c(0,.3)) 
print(ggplot_object)
dev.off()

pdf(file = "Sanders_pos_neg.pdf", 
    width = 7.5, height = 7.5, paper = "letter")  
ggplot_object<- ggplot(bs4, aes(x = quality, y = (total/2460))) + geom_bar(stat = "identity", aes(fill = quality)) + theme_fivethirtyeight() + guides(fill = FALSE) + ggtitle("Bernie Sanders tweets") + scale_y_continuous(breaks = seq(0,.7,.1), labels = c("", "10%", "20%", "30%", "40%", "50%", "60%", "70%"), limits = c(0,.75))
print(ggplot_object)
dev.off()


###### HILLARY CLINTON #######

# Read in the data
hc <- read.csv("hillaryclinton_tweets.csv", stringsAsFactors = FALSE)

# Create stop words, including campagin slogan
hc_stopwords <- c('Hillary', 'Clinton', 'hillaryclinton', 'hillary', 'clinton', 'imwithher')

# Pre-process the corpus to remove non-alphanumeric characters, links, numbers
# Remove punctuation, change to lower case, remove stopwords
hc_corpus <- Corpus(VectorSource(hc$tweet_text))
for(j in seq(hc_corpus))   
{   
  hc_corpus[[j]] <- gsub("[^[:alnum:] ]", "", hc_corpus[[j]])
  hc_corpus[[j]] <- gsub("http.*", "", hc_corpus[[j]])
  hc_corpus[[j]] <- gsub("*hillary", "", hc_corpus[[j]])
}
hc_corpus <- tm_map(hc_corpus, removeNumbers) 
hc_corpus <- tm_map(hc_corpus, tolower)
hc_corpus <- tm_map(hc_corpus, removePunctuation)
hc_corpus <- tm_map(hc_corpus, removeWords, c(stopwords("english"),hc_stopwords))
hc_corpus <- tm_map(hc_corpus, PlainTextDocument)

# Create a Document Term Matrix and convert it to a dataframe
hc_frequencies <- DocumentTermMatrix(hc_corpus)
hc_df <- as.data.frame(as.matrix(hc_frequencies))
#save document term matrix dataframe
write.csv(hc_df, "clintontweets_dtm.csv")

# Remove sparse terms -- terms that no appear in at least 0.5% of tweets
hc_sparse <- removeSparseTerms(hc_frequencies, 0.995)
hc_sparse <- as.data.frame(as.matrix(hc_sparse))
colnames(hc_sparse) = make.names(colnames(hc_sparse))

# Sort the sparsed dataframe by decreasing frequency and set the color levels for the word cloud
hc_wordFreqsort <- sort(colSums(hc_sparse),decreasing=T)
hc_grayLevels <- gray((hc_wordFreqsort + 10)/(max(hc_wordFreqsort)+10))

pdf(file = "Clinton_wordcloud.pdf", 
    width = 7.5, height = 7.5, paper = "letter")  
hc_wordCloud <- wordcloud(words=names(hc_wordFreqsort),freq=hc_wordFreqsort,min.freq=50,random.order=T,colors=hc_grayLevels)
dev.off()


# Cluster analysis
hc2 <- hc_sparse
hc2[hc2>1]=1

hcNorm <- hc2
hcNorm_Dist <- dist(hcNorm, method="manhattan") # compute distances
hcNorm_HC <- hclust(hcNorm_Dist, method="ward.D") # make model
plot(hcNorm_HC) # plot the dendogram
hcNorm_HC_Groups <- cutree(hcNorm_HC, k = 5) # split data into clusters

#Visualize the clusters
lapply(split(hc_sparse, hcNorm_HC_Groups), Top10Words)

#Create dataframe with word counts
hc_colsums <- colSums(hc_sparse)
hc_df_final <- cbind(colnames(hc_sparse), hc_colsums)
hc_df_final <- as.data.frame(hc_df_final)
rownames(hc_df_final) <- c()
colnames(hc_df_final) <- c('word', 'count')

hc_df_final <- merge(hc_df_final, emolex, by.x = "word", by.y = "word")
hc_df_final$count <- as.numeric(hc_df_final$count)
hc_df_final$total <- (hc_df_final$count)*(hc_df_final$yes)
hc3 <- filter(hc_df_final, yes == 1)
hc3 <- filter(hc3, quality != "positive")
hc3 <- filter(hc3, quality != "negative")
hc4 <- filter(hc_df_final, yes == 1)
hc4 <- filter(hc4, quality %in% c("positive", "negative"))

#Get total count for charts below
sum(hc3$total)
sum(hc4$total)

#Visualize the sentiments and emotions
pdf(file = "Clinton_emotions.pdf", 
    width = 7.5, height = 7.5, paper = "letter")  
ggplot_object <- ggplot(hc3, aes(x = quality, y = (total/3724))) + geom_bar(stat = "identity") + theme_fivethirtyeight() + ggtitle("Hillary Clinton tweets") + scale_y_continuous(breaks = seq(0,.3,.1), labels = c("", "10%", "20%", "30%"), limits = c(0,.3))
print(ggplot_object)
dev.off()

pdf(file = "Clinton_pos_neg.pdf", 
    width = 7.5, height = 7.5, paper = "letter")  
ggplot_object <- ggplot(hc4, aes(x = quality, y = (total/2299))) + geom_bar(stat = "identity", aes(fill = quality)) + theme_fivethirtyeight() + guides(fill = FALSE) + ggtitle("Hillary Clinton tweets") + scale_y_continuous(breaks = seq(0,.7,.1), labels = c("", "10%", "20%", "30%", "40%", "50%", "60%", "70%"), limits = c(0,.75))
print(ggplot_object)
dev.off()


##### DONALD TRUMP #######

# Read in the data
dt <- read.csv("realdonaldtrump_tweets.csv", stringsAsFactors = FALSE)

# Create stop words, including campagin slogan
dt_stopwords <- c('Donald', 'Trump', 'donaldtrump', 'donald', 'trump', 'realdonaldtrump', 'trump2016', 'makeamericagreatagain')

# Pre-process the corpus to remove non-alphanumeric characters, links, numbers
# Remove punctuation, change to lower case, remove stopwords
dt_corpus <- Corpus(VectorSource(dt$tweet_text))
for(j in seq(dt_corpus))   
{   
  dt_corpus[[j]] <- gsub("[^[:alnum:] ]", "", dt_corpus[[j]])
  dt_corpus[[j]] <- gsub("http.*", "", dt_corpus[[j]])
}
dt_corpus <- tm_map(dt_corpus, removeNumbers) 
dt_corpus <- tm_map(dt_corpus, tolower)
dt_corpus <- tm_map(dt_corpus, removePunctuation)
dt_corpus <- tm_map(dt_corpus, removeWords, c(stopwords("english"),dt_stopwords))
dt_corpus <- tm_map(dt_corpus, PlainTextDocument)

# Create a Document Term Matrix and convert it to a dataframe
dt_frequencies <- DocumentTermMatrix(dt_corpus)
dt_df <- as.data.frame(as.matrix(dt_frequencies))
#save document term matrix dataframe
write.csv(dt_df, "trumptweets_dtm.csv")

# Remove sparse terms -- terms that no appear in at least 0.5% of tweets
dt_sparse <- removeSparseTerms(dt_frequencies, 0.995)
dt_sparse <- as.data.frame(as.matrix(dt_sparse))
colnames(dt_sparse) = make.names(colnames(dt_sparse))

# Sort the sparsed dataframe by decreasing frequency and set the color levels for the word cloud
dt_wordFreqsort <- sort(colSums(dt_sparse),decreasing=T)
dt_grayLevels <- gray((dt_wordFreqsort + 10)/(max(dt_wordFreqsort)+10))

pdf(file = "Trump_wordcloud.pdf", 
    width = 7.5, height = 7.5, paper = "letter")  
dt_wordCloud <- wordcloud(words=names(dt_wordFreqsort),freq=dt_wordFreqsort,min.freq=50,random.order=T,colors=dt_grayLevels)
dev.off()


# Cluster analysis
dt2 <- dt_sparse
dt2[dt2>1]=1

dtNorm <- dt2
dtNorm_Dist <- dist(dtNorm, method="manhattan") # compute distances
dtNorm_HC <- hclust(dtNorm_Dist, method="ward.D") # make model
plot(dtNorm_HC) # plot the dendogram
dtNorm_HC_Groups <- cutree(dtNorm_HC, k = 7) # split data into clusters

# Visualize the clusters
lapply(split(dt_sparse, dtNorm_HC_Groups), Top10Words)

#Create dataframe with word counts
dt_colsums <- colSums(dt_sparse)
dt_df_final <- cbind(colnames(dt_sparse), dt_colsums)
dt_df_final <- as.data.frame(dt_df_final)
rownames(dt_df_final) <- c()
colnames(dt_df_final) <- c('word', 'count')

dt_df_final <- merge(dt_df_final, emolex, by.x = "word", by.y = "word")
dt_df_final$count <- as.numeric(dt_df_final$count)
dt_df_final$total <- (dt_df_final$count)*(dt_df_final$yes)
dt3 <- filter(dt_df_final, yes == 1)
dt3 <- filter(dt3, quality != "positive")
dt3 <- filter(dt3, quality != "negative")
dt4 <- filter(dt_df_final, yes == 1)
dt4 <- filter(dt4, quality %in% c("positive", "negative"))

#Get total count for charts below
sum(dt3$total)
sum(dt4$total)

# Visualize the sentiments and emotions
pdf(file = "Trump_emotions.pdf", 
    width = 7.5, height = 7.5, paper = "letter")  
ggplot_object <- ggplot(dt3, aes(x = quality, y = (total/3672))) + geom_bar(stat = "identity") + theme_fivethirtyeight() + ggtitle("Donald Trump tweets") + scale_y_continuous(breaks = seq(0,.3,.1), labels = c("", "10%", "20%", "30%"), limits = c(0,.3))
print(ggplot_object)
dev.off()

pdf(file = "Trump_pos_neg.pdf", 
    width = 7.5, height = 7.5, paper = "letter")  
ggplot_object <- ggplot(dt4, aes(x = quality, y = (total/1944))) + geom_bar(stat = "identity", aes(fill = quality)) + theme_fivethirtyeight() + guides(fill = FALSE) + ggtitle("Donald Trump tweets") + scale_y_continuous(breaks = seq(0,.7,.1), labels = c("", "10%", "20%", "30%", "40%", "50%", "60%", "70%"), limits = c(0,.75))
print(ggplot_object)
dev.off()


