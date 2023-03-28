##Title: Text Analysis of UN Peace Operations in Cyprus
##Date of Creation: 2 December 2021
##Author: Sara Kallis

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###Analysis of @UN_Cyprus Tweets####
UN_Cyprus_clean <- read.csv("UN_CYPRUS_3000_tweets.csv")
UN_Cyprus_clean$X <- NULL

####Text Analysis 1####
corpus <- VCorpus(VectorSource(UN_Cyprus_clean$text))

##Clean up corpus
# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#6. Stemming
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content #take a look

#7. Create Document Term Matrix
DTM <- DocumentTermMatrix(corpus)
inspect(DTM) 

#8. Data Visualisations
#8a. Create Word Cloud (use non-stemmed)
sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
sums <- sums[-c(1, 5, 12, 13, 15, 16, 20, 25, 26, 30, 33, 47, 48), ]   # remove amp, can, day, work, join, must, support, help, will, need, around, make, take
head <- sums[1:75,]
wordcloud(words = head$term, freq = head$count, min.freq = 100,
          max.words=30, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
?wordcloud

#8b. Bar Chart of Word Frequencies

sums %>% 
  mutate(term = fct_reorder(term, count)) %>%
  filter(count > 180) %>% 
  ggplot( aes(x=term, y=count)) +
  geom_bar(stat="identity", fill="seagreen", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  ggtitle("Twitter @UN_Cyprus: Word Frequency (2020-2021)")


#9. Sentiment Analysis
library(SentimentAnalysis)
sent <- analyzeSentiment(DTM, language = "english")
sent <- as.data.frame(sent)
summary(sent$SentimentGI)


####Text Analysis 2####
#### Limit sample of tweets to those including word(s) 'environment' and/or 'climate'
tweets_envi <- read.csv("tweets_envi.csv")
corpus <- VCorpus(VectorSource(tweets_envi$text))

#tweets_envi <- tweets_envi %>%
#filter(is_quote == FALSE, is_retweet == FALSE) #72 original tweets

##Clean up corpus
# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#7. Create Document Term Matrix
DTM <- DocumentTermMatrix(corpus)
sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
sums <- sums[-c(1), ]   # remove 'amp'
head <- sums[1:75,]

sums %>% 
  mutate(term = fct_reorder(term, count)) %>%
  filter(count > 40) %>% 
  ggplot( aes(x=term, y=count)) +
  geom_bar(stat="identity", fill="seagreen", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  ggtitle("Twitter @UN_Cyprus: Most Frequent Words")

###Analysis of @UNDPCY Tweets####
UNDPCY_clean <- read.csv("UNDPCY_clean_3000_tweets.csv")
UNDPCY_clean$X <- NULL
UNDPCY_clean$created_at <- as.Date(UNDPCY_clean$created_at)
UNDP_CY_2020_21 <- UNDPCY_clean %>%
  filter(created_at >= "2020-08-01") #145 tweets 1 August 2020- 14 December 2021

####Text Analysis 2####
corpus <- VCorpus(VectorSource(UNDPCY_clean$text))
#or
corpus <- VCorpus(VectorSource(UNDP_CY_2020_21$text))


##Clean up corpus
# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#7. Create Document Term Matrix
DTM <- DocumentTermMatrix(corpus)
inspect(DTM) 

sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))

sums <- sums[-c(1, 6, 9, 10, 12, 14, 15, 16, 19, 20, 25, 27, 32), ]   # for all year: remove amp, work, new, today, join, day, support, , works, will, undpff, now, can, apply
sums <- sums[-c(1, 5, 6, 7, 8, 16, 21), ]   # for 2020-21: remove amp, works, funded, support, will, work, funding

head <- sums[1:75,]

sums %>% 
  mutate(term = fct_reorder(term, count)) %>%
  filter(count > 15) %>% 
  ggplot( aes(x=term, y=count)) +
  geom_bar(stat="identity", fill="darkblue", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  ggtitle("Twitter @UNDPCY: Word Frequency (2020-2021)")

