#Removing any pre-defined variables
rm(list=ls())

library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(dplyr)
library(ggplot2)
library(igraph)

data <- c('SCdata.txt', 'MKdata.txt')
text <- lapply(unlist(lapply(data, readLines)), readLines)
#text <- readLines('Sociocultural_perspective.txt')

#text <- iconv(text, "CP949", "UTF-8")
text <- stringr::str_replace_all(text, "\\W", " ")

myCorpus <- Corpus(VectorSource(text))

cleanCorpus <- function(x=myCorpus) {
x <- tm_map(x, content_transformer(tolower))
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

myCorpus <- cleanCorpus(myCorpus)

#Dictionary below
#myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
#myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)
#myCorpus <- tm_map(myCorpus, gsub, pattern="miners", replacement="mining")

# Define modified stemCompletion function
stemCompletion2 <- function(x, dict=dictionary) {
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x), " ")), dictionary=dict, type="shortest"),sep="", collapse=" ")))
}

myCorpus <- lapply(myCorpus, stemCompletion2, dict=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))

myCorpus <- cleanCorpus(myCorpus)
class(myCorpus)

# replacement function for Learning Sciences
replaceLS <- function(x) {
rpLS <- as.data.frame(read.csv("replacing/LS.csv", row.names=NULL), stringsAsFactors=F)

    xCopy <- x
    for(i in 1:nrow(rpLS)) {
        xCopy <- gsub(toString(rpLS[i, 1]), toString(rpLS[i, 2]), xCopy)
    }
    return(x <- xCopy)
}
myCorpus <- tm_map(myCorpus, replaceLS)

# TfIdf
tdm <- TermDocumentMatrix(myCorpus, control = list(tolower=F, weighting=weightTfIdf))
tdm_df <- as.data.frame(as.matrix(tdm), stringsAsFactors=F)
write.csv(tdm_df, "output/TDM/TfIdf.csv")

wordFreq <- sort(rowSums(tdm_df), decreasing=T)
class(wordFreq)
wordFreq <- as.data.frame(as.matrix(wordFreq), stringsAsFactors=F)
wordFreq$new <- rownames(wordFreq)
rownames(wordFreq) <- 1:nrow(wordFreq)
wordFreq <- wordFreq[,c(2,1)]
colnames(wordFreq) <- c("word", "TfIdf")
write.csv(wordFreq, "output/CSV/TfIdf.csv")

# Setting colors
pal1 <- brewer.pal(8, "Dark2")
pal2 <- brewer.pal(9, "Blues")[5:9]

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

# Common
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

corAnalysis(term="makerspace", cor=0.7)

top20 <- wordFreq2 %>%
  arrange(desc(freq)) %>%
  head(20)

order <- arrange(top20, freq)$word
maxNum <- (50+as.numeric(top20[1,2]))

pdf("output/GGplot/Frequency.pdf")
ggplot(data = top20, aes(x = word, y = freq)) +  
  ylim(0, maxNum) +
  geom_col(fill="lightgreen", color="green", width=0.8) + 
  coord_flip() +
  scale_x_discrete(limit = order) +              
  geom_text(aes(label = freq), hjust=-0.3)     
dev.off()

