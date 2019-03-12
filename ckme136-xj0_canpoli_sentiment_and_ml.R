### == CKME136 XJ0 ==
## RScript #3- Sentiment analysis and machine learning

#Load packages
library(tidyverse) #For wrangling data
library(tidytext) #For extra wrangling capabilities for text data
library(syuzhet) #For sentiment analysis


#Set working directory and path for saving CSV
wd=setwd("B:\\canpoli_ryerson")
path="B:\\canpoli_ryerson"

### -------------- LOAD FILTERed DATA SETS-------------- 

#Load filtered get_timelines data set
canpoli.tmls.filtered=read_csv("canpoli.tmls.filtered.csv") 

#Load filtered search_tweets data set 
canpoli.srch.filtered=read_csv("canpoli.srch.filtered.csv") 


### -------------- SENTIMENT ANALYSIS (nrc) ON TWEET_TIMELINE DATA (TEXT) -------------- 

#Filter on greater than Jan 01, 2019, not including retweets, and English tweets only
canpoli.tmls.filtered= canpoli.tmls.filtered %>% dplyr::filter(created_at > "2019-01-01", lang=="en", is_retweet=="FALSE")
canpoli.srch.filtered= canpoli.srch.filtered %>% dplyr::filter(created_at > "2019-01-01", lang=="en", is_retweet=="FALSE")


#Get semtiment =====> Will analyze sentiment for each leader's tweets and public sentiment towards each leader

#Get the nrc (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) for each group of tweet text 
sentiment_tmls=get_nrc_sentiment(canpoli.tmls.filtered$text)
sentiment_srch=get_nrc_sentiment(canpoli.srch.filtered$text)

#Create a data frame of the sentiment analysis [timelines]
sentiment_df=data.frame(colSums(sentiment_tmls[,]))
names(sentiment_df)="Score"
sentiment_df=cbind("Sentiment" =rownames(sentiment_df),sentiment_df)

#Plot sentiment analysis to a bar chart [timelines]
ggplot(data=sentiment_df,aes(x=Sentiment,y=Score))+
  geom_bar(aes(fill=Sentiment),stat="identity")+
  theme(legend.position="none")+
  coord_flip()


#Create a data frame of the sentiment analysis [search]
sentiment_df=data.frame(colSums(sentiment_srch[,]))
names(sentiment_df)="Score"
sentiment_df=cbind("Sentiment" =rownames(sentiment_df),sentiment_df)

#Plot sentiment analysis to a bar chart [search]
ggplot(data=sentiment_df,aes(x=Sentiment,y=Score))+
  geom_bar(aes(fill=Sentiment),stat="identity")+
  theme(legend.position="none")+
  coord_flip()


### -------------- SENTIMENT ANALYSIS (positive/negative) ON TWEET_TIMELINE DATA (TEXT) -------------- 
##TO BE COMPLETED - MACHINE LEARNING MODEL

#Classification tree

