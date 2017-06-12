library(twitteR)
library(ROAuth)
library(httr)
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(sentiment)
library(stringr)
library(plyr)
install.packages("sentiment")

key = "cy8C5r8jqFCxUXZ9CgLHJF5Nz" #Twitter Key
secret = "retEYMMWXXAmgpBVhC6i50IEc3mWPjlROiTJvCP9MXsDReDRob" #Secret 
setwd("C:/Users/sreejay/Desktop/sem2/Udemy/text mining")
#download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "C:/Users/sreejay/Desktop/sem2/Udemy/text mining/cacert.pem", method = "auto")
authenticate <- setup_twitter_oauth(consumer_key = key, consumer_secret = secret,
                                    access_secret = "kpxqLXvGirlHXOtsmONAmquAaeswueVYiYeLbr1lYpIRt",
                                  access_token = "83341637-5jvAwCLc9xomILdE576JHA72MWU2LQMiJUNHrVfUy")
udemytwitter <- searchTwitter("Trump", n= 2000)
head(udemytwitter)
udemytweets = searchTwitter("#SachinTendulkar", n =1000)
userTimeline("Trump")
#getting text from the the data
udemytext <- sapply(udemytweets,function(x) x$getText())

head(udemytext)
udemycorpus <- Corpus(VectorSource(udemytext))
head(udemycorpus)
udemycorpus <- tm_map(ipl,removePunctuation)
udemycorpus <- tm_map(ipl,tolower)
udemycorpus <- tm_map(ipl,function(x) removeWords(x,stopwords()))
udemycorpus1 <- tm_map(udemycorpus,PlainTextDocument)
?wordcloud
?RColorBrewer
display.brewer.pal()
col <- brewer.pal(5,"Pastel1")
wordcloud(RCBtweets_text,min.freq = 3, scale = c(5,1),random.color = F,rot.per = .9,colors = col,max.words = 30,random.order = F)
udemytdp <-TermDocumentMatrix(udemycorpus1)
udemytdp
findFreqTerms(udemytdp,lowfreq = 50)
findAssocs(udemytdp,'apple',.60)
udemy2tdm <- removeSparseTerms(udemytdp,sparse = .9)
udemy2tdmscale <- scale(udemy2tdm)
udemy2tdmscale
udemydist <- dist(udemy2tdmscale, method = "euclidean")
udemyclust <- hclust(udemydist)
plot(udemyclust)
cutree(udemyclust, k=7)
rect.hclust(udemyclust,k=7,border = "red")

strsplit("Hieaf",NULL)


#Sentimental Analysis
getwd()
pos = readLines("Positive-Words.txt")
Neg = readLines("Negative-Words.txt")

mytest <- c('I am a super good guy',"awesome experience", "you are so stupid","are you i o u")
head(pos)

test = score.sentiment(mytest,pos,Neg)
test
score.sentiment = function(sentences,pos.words,Neg.words, .progress="none")
{
  scores = laply(sentences,function(sentence,pos.words,Neg.words)
    {
    sentence = gsub("[[:punct:]]","",sentence)
    
    sentence = gsub("[[:cntrl:]]","",sentence)
    sentence = gsub('\\d+','',sentence)
    trytolower = function(x)
    {
      y = NA
      try_error = tryCatch(tolower(x),error=function(e) e)
      if(!inherits(try_error,"error"))
        y = tolower(x)
      return(y)
      
    }
    sentence =  sapply(sentence,trytolower)
    word.list = str_split(sentence,'\\s+')
    words=unlist(word.list)
    pos.matches = match(words,pos.words)
    neg.matches = match(words,Neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score =  sum(pos.matches) - sum(neg.matches)
    return(score)
    
  },pos.words,Neg.words,.progress = .progress)
  scores.df = data.frame(text=sentences,score = scores)
  return(scores.df)
}
RCBtweets <-  searchTwitter("#RCB",n=1000,lang = "en")
MItweets <-  searchTwitter("#MI",n=1000,lang = "en")
CSKtweets <-  searchTwitter("#CSK",n=1000,lang = "en")
DCtweets <-  searchTwitter("#DC",n=1000,lang = "en")
SRHtweets <-  searchTwitter("#SRH",n=1000,lang = "en")
KKRtweets <-  searchTwitter("#KKR",n=1000,lang = "en")
RPStweets <-  searchTwitter("#RPS",n=1000,lang = "en")
DDtweets <-  searchTwitter("#DD",n=1000,lang = "en")
DDtweets

RCBtweets_text <- sapply(RCBtweets,function(x) x$getText())
MItweets_text <- sapply(MItweets,function(x) x$getText())
DCtweets_text <- sapply(DCtweets,function(x) x$getText())
CSKtweets_text <- sapply(CSKtweets,function(x) x$getText())
SRHtweets_text <- sapply(SRHtweets,function(x) x$getText())
KKRTweets_text <- sapply(KKRtweets,function(x) x$getText())
RPStweets_text <- sapply(RPStweets,function(x) x$getText())
DDtweets_text <- sapply(DDtweets,function(x) x$getText())

NP <- c(length(RCBtweets_text),length(MItweets_text),length(DCtweets_text),length(CSKtweets_text),length(SRHtweets_text)
        ,length(KKRTweets_text),length(RPStweets_text),length(DDtweets_text))

ipl <- c(RCBtweets_text,MItweets_text,DCtweets_text,CSKtweets_text,SRHtweets_text,KKRTweets_text,RPStweets_text,DDtweets_text)
scores = score.sentiment(ipl,pos,Neg,.progress = 'text')
scores$ipl <- factor(rep(c("RCB","MI","DC","CSK","SRH","KKR","RPS","DD"),NP))
scores$pos_tweet = as.numeric(scores$score >= 1)
scores$neutral_tweet = as.numeric(scores$score == 0)


scores$neg_tweet = as.numeric(scores$score <= -1)
numpos = sum(scores$pos_tweet)
numneg = sum(scores$neg_tweet)
sum(scores$neutral_tweet)

#GLOBAL score 
global_score = round(100 * numpos/(numpos + numneg))
head(scores)
boxplot(score~ipl,data = scores)
library("lattice")
histogram(data=scores, ~score|ipl)
