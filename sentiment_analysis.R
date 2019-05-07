
dt <- read.csv("tweetsdata_clean.csv", sep = ";")

library(quanteda)
library(SentimentAnalysis)
library(GGally)
library(ggplot2)


data("DictionaryGI")
DictionaryGI$negative[1:30]
DictionaryGI$positive[1:30]
DictionaryHE$negative[1:30]
DictionaryLM$negative[1:30]

qdap <- loadDictionaryQDAP()
qdap$negativeWords[1:30]

data_dictionary_LSD2015$negative[1:30]

sentiments <- analyzeSentiment(as.character(dt$text))
tokenized <- tokens_lookup(tokens(as.character(dt$text)), dictionary=data_dictionary_LSD2015, exclusive=FALSE) 
sentiments$LCpos <- sapply(tokenized, function(x) {sum(x=='POSITIVE') - sum(x=='NEG_POSITIVE') + sum(x=='NEG_NEGATIVE')})
sentiments$LCneg <- sapply(tokenized, function(x) {sum(x=='NEGATIVE') - sum(x=='NEG_NEGATIVE') + sum(x=='NEG_POSITIVE')})
sentiments$LC <- (sentiments$LCpos-sentiments$LCneg)/sentiments$WordCount

ggpairs(data.frame(sentiments$SentimentHE, sentiments$SentimentGI, sentiments$SentimentQDAP, sentiments$SentimentLM, sentiments$LC))

ggpairs(data.frame(sentiments$SentimentHE, sentiments$SentimentQDAP, sentiments$LC))

ggplot(data=data.frame(time=dt$created, sentiment=sentiments$LC), aes(x=as.numeric(time), y=sentiment)) + geom_point(size = 0.05 ) + geom_smooth()
ggplot(data=data.frame(time=as.Date(dt$created_at), sentiment=sentiments$LC), aes(x=as.Date(time), y=sentiment)) + geom_boxplot()

ggplot(data=data.frame(time=as.Date(dt$created_at), sentiment=sentiments$LC), aes(x = as.factor(time), y = sentiment)) + geom_boxplot() + geom_smooth()

dt$GIsent = ifelse(sentiments$LC==0, 'neutral', ifelse(sentiments$LC>0, 'positive', 'negative')) 
ggplot(dt, aes(as.Date(created_at), fill=dt$GIsent)) + geom_histogram()
ggplot(dt, aes(as.Date(created_at), fill=GIsent)) + geom_histogram(position='fill')

par(mfrow=c(1,2)) 
hist(sentiments$SentimentGI[dt$is_retweet==FALSE], main='Sentiment of Original Tweets', xlab='Sentiment')
hist(sentiments$SentimentGI[dt$is_retweet==TRUE], main='Sentiment of Retweets', xlab='Sentiment') 

