#Removing any pre-defined variables
rm(list=ls())

library(twitteR)
library(plyr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(dplyr)
library(ggplot2)
library(igraph)

# Setting colors
pal1 <- brewer.pal(8, "Dark2")
pal2 <- brewer.pal(9, "Blues")[5:9]

# Predefine functions:

# 1. Cleaning Function:
cleanCorpus <- function(x=myCorpus) {
removeComment <- function(x) gsub("#[[:alnum:]]*", "", x)
x <- tm_map(x, removeComment)

x <- tm_map(x, content_transformer(tolower))

removeAt <- function(x) gsub("@[[:alnum:]]*", "", x)
x <- tm_map(x, removeAt)

x <- tm_map(x, stripWhitespace)
x <- tm_map(x, removePunctuation)
x <- tm_map(x, removeNumbers)

#Add exception word list
except <- readLines("exception.txt")
cnt_txt <- length(except)
myStopwords <- c(stopwords('english'))
for(i in 1:cnt_txt) {
	myStopwords <- append(myStopwords, except[i])
}

# remove stopwords from corpus
x <- tm_map(x, removeWords, myStopwords)

# remove URLs
 removeWWW <- function(x) gsub("www[[:alnum:]]*", "", x)
x <- tm_map(x, removeWWW)

 removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
x <- tm_map(x, removeURL)
return(x)
}

# 2. Define modified stemCompletion function
stemCompletion2 <- function(x, dict=dictionary) {
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x), " ")), dictionary=dict, type="shortest"),sep="", collapse=" ")))
}

# 3. replacement function for Learning Sciences
replaceLS <- function(x) {
rpLS <- as.data.frame(read.csv("replacing/LS.csv", row.names=NULL), stringsAsFactors=F)

    xCopy <- x
    for(i in 1:nrow(rpLS)) {
        xCopy <- gsub(toString(rpLS[i, 1]), toString(rpLS[i, 2]), xCopy)
    }
    return(x <- xCopy)
}

# 4. Correlation analysis function
corAnalysis <- function(term, cor) {
df <- as.data.frame(findAssocs(tdm2, terms=term, corlimit=cor)[[term]], stringsAsFactors=F)
df$new <- rownames(df)
rownames(df) <- 1:nrow(df)
df <- df[,c(2,1)]
colnames(df) <- c("word", "correlation")
write.csv(df, "output/CSV/Correlation.csv")

top20_cor <- df %>%
  arrange(desc(correlation)) %>%
  head(20)

# Network graph
pdf("output/NetWork/Correlation.pdf")
top20_cor <- rename(top20_cor, terms=word, probs=correlation)
g <- graph.data.frame(top20_cor, directed = F)
plot(g) 
dev.off()	

 # word cloud
pdf("output/WordCloud/Correlation.pdf")
 set.seed(375) # to make it reproducible
 wordcloud(
words=df$word,
freq=df$correlation,
scale = c(6, 0.2),
min.freq=10,
max.words=200,
rot.per = .1,
random.order=F,
colors = pal2
)
 dev.off()
}

# 5. Sentimental analysis


score.sentiment = function(sentences, pos.words, neg.words)
{
   # Parameters
   # sentences: vector of text to score
   # pos.words: vector of words of postive sentiment
   # neg.words: vector of words of negative sentiment  
   # create simple array of scores with laply

   scores = laply(sentences, 
   function(sentence, pos.words, neg.words)
   {
      # remove punctuation
      sentence = gsub("[[:punct:]]", "", sentence)
      # remove control characters
      sentence = gsub("[[:cntrl:]]", "", sentence)
      # remove digits?
      sentence = gsub('\\d+', '', sentence)

      # define error handling function when trying tolower
      tryTolower = function(x)
      {
         # create missing value
         y = NA
         # tryCatch error
         try_error = tryCatch(tolower(x), error=function(e) e)
         # if not an error
         if (!inherits(try_error, "error"))
         y = tolower(x)
         # result
         return(y)
      }

      # use tryTolower with sapply 
      sentence = sapply(sentence, tryTolower)
      # split sentence into words with str_split (stringr package)
      word.list = str_split(sentence, "\\s+")
      words = unlist(word.list)

    # compare words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)

      # get the position of the matched term or NA
      # we just want a TRUE/FALSE
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)

      # final score
      score = sum(pos.matches) - sum(neg.matches)
      return(score)
    }, pos.words, neg.words)
# data frame with scores for each sentence
   scores.df = data.frame(text=sentences, score=scores)
   return(scores.df)
}

# import positive and negative words
pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')

##Only use the following lines for authentication for interactive mode as it requires browsers.
#api_key <- "jUuc9y7KO93KjvLLCrz8dKC8o"
#api_secret <- "0J8oePTg9vYC9WdgD9w5MgXBGH4hzZe8IGefS4jvto7gGQDkxT"
#access_token <- "222434166-17JmJoJXEbBq4a655OLfkGSnQLkKoLHrBq2Qg6Hk"
#access_token_secret <- "1FuLSaJfl5HQcpo3MLYemYVFs1GBAXIMd2IqifGRG1FEl"
#setup_twitter_oauth(api_key, api_secret)

##Authentication for terminal mode.
#setup_twitter_oauth(consumer_key = "jUuc9y7KO93KjvLLCrz8dKC8o", consumer_secret = "0J8oePTg9vYC9WdgD9w5MgXBGH4hzZe8IGefS4jvto7gGQDkxT")
#token <- get("oauth_token", twitteR:::oauth_cache)
#token$cache()
#save(token, "key.RData")
load("key.Rdata")
twitteR::use_oauth_token(token)

#twUser = "dalepd"
#outputTweets <- userTimeline(twUser, n=1000)
#twTerm <- "makerspace"
#outputTweets <- searchTwitter(twTerm, n=10000, lang="en")

#head(outputTweets)
#ls()
#str(rdmTweets)

#df <- do.call("rbind", lapply(outputTweets, as.data.frame))
#write.csv(df$text, "outputTweetsData.txt")
#write.csv(df$text, "outputTweetsData.csv")

text <- readLines("outputTweetsData.txt")

# 1. Creating corpus
myCorpus <- Corpus(VectorSource(text))

# 2. Cleaning corpus
myCorpus <- cleanCorpus(myCorpus)
class(myCorpus)
str(myCorpus)

# 3. corpus Stemming
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- lapply(myCorpus, stemCompletion2, dict=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))

# 4. Cleaning corpus again
myCorpus <- cleanCorpus(myCorpus)
myCorpus <- tm_map(myCorpus, replaceLS)
class(myCorpus)
str(myCorpus)


# TfIdf
tdm <- TermDocumentMatrix(myCorpus, control = list(tolower=F, weighting=weightTfIdf))
tdm_df <- as.data.frame(as.matrix(tdm), stringsAsFactors=F)
write.csv(tdm_df, "output/TDM/TfIdf.csv")

wordFreq <- sort(rowSums(tdm_df), decreasing=T)
wordFreq <- as.data.frame(as.matrix(wordFreq), stringsAsFactors=F)
wordFreq$new <- rownames(wordFreq)
rownames(wordFreq) <- 1:nrow(wordFreq)
wordFreq <- wordFreq[,c(2,1)]
colnames(wordFreq) <- c("word", "TfIdf")
write.csv(wordFreq, "output/CSV/TfIdf.csv")

 # word cloud
pdf("output/WordCloud/TfIdf.pdf")
 set.seed(375) # to make it reproducible
 wordcloud(
words=wordFreq$word,
freq=wordFreq$TfIdf,
scale = c(6, 0.2),
min.freq=10,
max.words=200,
rot.per = .1,
random.order=F,
colors = pal2
)
 dev.off()

# General TDM
tdm2 <- TermDocumentMatrix(myCorpus, control = list(tolower=F))
tdm_df2 <- as.data.frame(as.matrix(tdm2), stringsAsFactors=F)
write.csv(tdm_df2, "output/TDM/TDM.csv")


wordFreq2 <- sort(rowSums(tdm_df2), decreasing=T)
wordFreq2 <- as.data.frame(as.matrix(wordFreq2), stringsAsFactors=F)
wordFreq2$new <- rownames(wordFreq2)
rownames(wordFreq2) <- 1:nrow(wordFreq2)
wordFreq2 <- wordFreq2[,c(2,1)]
colnames(wordFreq2) <- c("word", "freq")
write.csv(wordFreq2, "output/CSV/Frequency.csv")

 # word cloud
pdf("output/WordCloud/Frequency.pdf")
 set.seed(375) # to make it reproducible
 wordcloud(
words=wordFreq2$word,
freq=wordFreq2$freq,
scale = c(6, 0.2),
min.freq=10,
max.words=200,
rot.per = .1,
random.order=F,
colors = pal2
)
dev.off()

top20 <- wordFreq2 %>%
  arrange(desc(freq)) %>%
  head(20)

order <- arrange(top20, freq)$word
maxNum <- (30+as.numeric(top20[1,2]))

pdf("output/GGplot/Frequency.pdf")
ggplot(data = top20, aes(x = word, y = freq)) +  
  ylim(0, maxNum) +
  geom_col(fill="lightgreen", color="green", width=0.8) + 
  coord_flip() +
  scale_x_discrete(limit = order) +              
  geom_text(aes(label = freq), hjust=-0.3)     
dev.off()

corAnalysis(term="makerspace", cor=0.7)

data.score = score.sentiment(readLines(wordFreq2$word), pos.words, neg.words)
#class(data.score)
#str(data.score)

#data.score$score
table(data.score$score)
mean(data.score$score)

pdf("histogram.pdf")
hist(data.score$score, main="Sentimental Analysis of Tweets", col="pink", ylab="Sentimental Score")
dev.off()

pdf("ggplot.pdf")
qplot(data.score$score)
dev.off()

# NetWork Graph
tdm3 <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(2, Inf)))
mydata.tdm <- removeSparseTerms(tdm3, sparse=0.95)

mydata.df <- as.matrix(mydata.tdm)
mydata.df[mydata.df>=1] <- 1

mydata.df2 <- mydata.df %*% t(mydata.df)

# Visualization
pdf("output/NetWork/igraph.pdf")
data.g <- graph.adjacency(mydata.df2, weighted=TRUE, mode="undirected")
g <- simplify(data.g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
V(g)$label.cex <- 2*(V(g)$degree / max(V(g)$degree))
V(g)$size <- 15*(V(g)$degree / max(V(g)$degree))
E(g)$width <- 4*(E(g)$weight / max(E(g)$weight))
condition <- V(g)[degree<10]
g1<-delete.vertices(g,condition)
head(sort(degree(g1),decreasing=T))
set.seed(1234)
plot(g1)
dev.off()
