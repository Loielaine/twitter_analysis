library(readr)
data<-read.csv2("/Users/Loielaine/Desktop/Good_Good_Study/2019Winter/SURVMTH622/hw2/tweetsdata.csv",sep=",")


#method 1
### filter irrelevant tweets 
# create your own lists of irrelevant words:
irrelevantWords = c('football','womens','Endgame','zealand','waseem','pray','breaking','praying','Muslim','prayers','police','praying','prayers','nz','attack','killed','religion')

michTweets = data.frame()
irrelevantTweets = data.frame()


# take a random sample of tweets of size n.sample
for(i in 1:12){
  n.sample = 5000
  bballtweets = data[n.sample*(i-1):(max(n.sample*i,nrow(data))),]
  for(i in 1:n.sample){
    # break up tweet into indiviaul words
    tweet = bballtweets$text[i]
    tweet = tolower(tweet)
    words = tokens(tweet)
    # if it contains an irrelevant word, put it in the irrelevant data frame
    if(length(intersect(words$text1, irrelevantWords))>0){
      irrelevantTweets = rbind(irrelevantTweets, bballtweets[i,])
    }
    else{
      michTweets = rbind(michTweets, bballtweets[i,])
    }
  }
}

#method 2

head(data$text,n=5)
id1<-grep("Waseem", data$text)
id2<-grep("pray", data$text)
id3<-grep("breaking", data$text)
id4<-grep("praying",data$text)
id5<-grep("Muslim",data$text)
id6<-grep("Zealand",data$text)
id7<-grep("BREAKING",data$text)
id8<-grep("prayers",data$text)
id9<-grep("police",data$text)
id10<-grep("Praying",data$text)
id11<-grep("Prayers",data$text)
id12<-grep('football',data$text)
id13<-grep('women',data$text)
id14<-grep('Endgame',data$text)
id15<-grep('nz',data$text)
id16<-grep('attack',data$text)
id17<-grep('kill',data$text)
id18<-grep('religion',data$text)
id19<-grep('Cinema',data$text)
id20<-grep('woman',data$text)
id21<-grep('gun',data$text)
dataclean<-data[-c(id1,id2,id3,id4,id5,id6,id7,id8,id9,id10,id11,
                   id12,id13,id14,id15,id16,id17,id18,id19,id20),]
sample(dataclean$text,5)
write.csv2(dataclean,file="/Users/Loielaine/Desktop/Good_Good_Study/2019Winter/SURVMTH622/hw2/tweetsdata_clean.csv")




